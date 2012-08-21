def wider(x: Box, y: Box): Box =
  greaterBy(x, y, _.width)

def taller(x: Box, y: Box) =
  greaterBy(x, y, _.height)