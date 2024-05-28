import os, stats, sequtils
import nimPNG
import snip

#######################################
# Load png file as grid2D
#######################################

func getPngGrid(pngres: PNGRes): Result[Grid2D[seq[uint8]], CatchableError] =
  let
    png = pngres.get()
    data = png.data
    w = png.width
    h = png.height
  var val = newSeqWith(w*h, newSeq[uint8](4))

  for i, x in data.pairs:
    val[i div 4][i mod 4] = x

  let grid = ?spawnGrid2D[seq[uint8]](val, w, h)
  return grid.ok()

func getPng(self: Grid2D[seq[uint8]]): seq[uint8] =
  var ans: seq[uint8]
  for x in self.pts.items():
    ans.add(x)
  return ans

#######################################


#######################################
# Binarization
#######################################

type
  Exploit = enum
    Over
    Above
    Below
    Under

type
  ConfigBinarization = object
    channel: array[4, bool]
    exploit: array[4, Exploit]
    threshold: array[4, uint8]

func binarizeCh(self: Grid2D[seq[uint8]], chNum: int, threshold: uint8, exploit: Exploit): Result[Grid2D[bool], CatchableError] =
  var ans = newSeq[bool](self.xlen * self.ylen)
  for i, x in self.pts.pairs():
    case exploit
    of Over:
      if x[chNum] > threshold:
        ans[i] = true
      else:
        ans[i] = false
    of Above:
      if x[chNum] >= threshold:
        ans[i] = true
      else:
        ans[i] = false
    of Below:
      if x[chNum] <= threshold:
        ans[i] = true
      else:
        ans[i] = false
    of Under:
      if x[chNum] < threshold:
        ans[i] = true
      else:
        ans[i] = false
  return (?spawnGrid2D(ans, self.xlen, self.ylen)).ok()

func getBinaryFilter(self: Grid2D[seq[uint8]], config: ConfigBinarization): Result[Grid2D[bool], CatchableError] =
  var binaryFilter = ?spawnGrid2D(newSeqWith(len(self.pts), true), self.xlen, self.ylen)
  for i in 0..<len(config.channel):
    if config.channel[i]:
      let filter = ?self.binarizeCh(i, config.threshold[i], config.exploit[i])
      for i, x in filter.pts.pairs:
        binaryFilter.pts[i] = binaryFilter.pts[i] and filter.pts[i]

  return binaryFilter.ok()

#######################################



#######################################
# Count grains in binary filter
#######################################

proc countGrain(self: var Grid2D[bool]): seq[seq[int]] =
  var
    grains = newSeqWith(0, newSeq[int](0))
    grainNum = -1
  for idx, px in self.pts.pairs():
    if px:
      grains.add(@[idx]) # generate grain as 'grains' element and probe pixel idx
      grainNum += 1
      let
        here = self.getPos(idx).value()
        neighbors = [ZP, PP, PZ, PN, ZN, NN ,NZ, NP]
      var
        stack = @[here]
      while stack.len() != 0:
        let pos = stack.pop()
        let res = self.getVal(pos)
        if res.isOk() and res.value:
          self.pts[self.getIdx(pos).value] = false
          for dir in neighbors:
            let nextPos = self.getNextPos(pos, dir)
            if nextPos.isOk():
              let nextRes = self.getVal(nextPos.value)
              if nextRes.isOk() and nextRes.value:
                let nextIdx = self.getIdx(pos)
                if nextIdx.isOk():
                  stack.add(nextPos.value)
                  grains[grainNum].add(nextIdx.value)

  return grains
      
#######################################

#######################################
# seq[bool] to seq[seq[uint8]]
#######################################

func getBinarizedImg(self: Grid2D[bool], a: uint8 = 255'u8): Grid2D[seq[uint8]] =
  var ans = spawnGrid2D(newSeqWith(len(self.pts), @[0'u8, 0'u8, 0'u8, a]), self.xlen, self.ylen).value()
  for idx, px in self.pts.pairs():
    if px:
      ans.pts[idx] = @[255'u8, 255'u8, 255'u8, a]

  return ans


#######################################
# Test
#######################################
proc main() =
  let pngres = loadPNG32(seq[uint8], "/home/lune/Pictures/test.png")
  if pngres.isErr:
    echo "input file is invalid!"
  else:
    let ares = getPngGrid(pngres)
    if ares.isErr():
      echo ares.error
    else:
      let a = ares.get()
      # echo a.xlen
      # echo a.ylen
      # echo len(a.pts)

      let config = ConfigBinarization(
        channel: [true, true, false, false],
        exploit: [Under, Under, Over, Over],
        threshold: [20, 20, 90, 90],
      )
      let bin = a.getBinaryFilter(config)
      if bin.isErr():
        echo bin.error
      else:
        let ans = bin.value
        # echo ans.xlen
        # echo ans.ylen
        # echo len(ans.pts)

        var anscut = ans
        # anscut.pts = anscut.pts[0..<7900]
        # anscut.xlen = 100
        # anscut.ylen = 79
        let
          grains = anscut.countGrain()

        var
          probe = 0
          grainsSorted = newSeqWith(0, newSeq[int](0))
        grainsSorted.add(grains[0])
        # 降順ソート
        while probe < len(grains):
          for i in 0..<len(grainsSorted):
            if len(grains[probe]) < len(grainsSorted[i]):
              grainsSorted.insert(grains[probe], i)
              probe += 1
              echo i
              echo len(grains)
              break
            elif i == len(grainsSorted)-1:
              grainsSorted.add(grains[probe])
              probe += 1
              echo i
              echo len(grains)
              break

        for i in 0..<len(grainsSorted):
          echo len(grainsSorted[i])

        let png = ans.getBinarizedImg(a = 255'u8)
        echo savePNG32("/home/lune/Pictures/test_bin.png", png.getPng(), png.xlen, png.ylen)
        echo "hoge"

main()

#######################################

