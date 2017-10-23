const canvas = document.getElementById('canvas')
const canvasWidth = canvas.width
const canvasHeight = canvas.height

let keys = {}

const UP_KEY = 38,
      RIGHT_KEY = 39,
      DOWN_KEY = 40,
      LEFT_KEY = 37;

let lastKey = null
canvas.addEventListener('keydown', ev => {
  keys[ev.keyCode] = ev.keyCode !== lastKey
  lastKey = ev.keyCode
})

canvas.addEventListener('keyup', ev => {
  lastKey = null
  delete keys[ev.keyCode]
})

const context = canvas.getContext('2d')

let worldMap =
  [ [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
  , [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
  , [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
  , [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
  , [1,0,0,0,0,0,2,2,2,2,2,0,0,0,0,3,0,3,0,3,0,0,0,1]
  , [1,0,0,0,0,0,2,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,1]
  , [1,0,0,0,0,0,2,0,0,0,2,0,0,0,0,3,0,0,0,3,0,0,0,1]
  , [1,0,0,0,0,0,2,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,1]
  , [1,0,0,0,0,0,2,2,0,2,2,0,0,0,0,3,0,3,0,3,0,0,0,1]
  , [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
  , [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
  , [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
  , [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
  , [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
  , [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
  , [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
  , [1,4,4,4,4,4,4,4,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
  , [1,4,0,4,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
  , [1,4,0,0,0,0,5,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
  , [1,4,0,4,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
  , [1,4,0,4,4,4,4,4,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
  , [1,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
  , [1,4,4,4,4,4,4,4,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
  , [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
]

let frame = 0

let pos = { x: 12, y: 12 }
let dir = { x: -1, y: 0 }
let plane = { x: 0, y: 0.66 }

function oob(pos) {
  return pos.x < 0
    || pos.x >= worldMap[0].length
    || pos.y < 0
    || pos.y >= worldMap.length
}

function draw() {
  ++frame

  // Update player every MOVE_SPEED frames
  if (keys[UP_KEY]) {
    pos.x += dir.x
    pos.y += dir.y
  } else if (keys[DOWN_KEY]) {
    pos.x -= dir.x
    pos.y -= dir.y
  }

  if (keys[RIGHT_KEY]) {
    // Both camera direction and camera plane must be rotated
    const oldDirX = dir.x
    dir.x = dir.y
    dir.y = -oldDirX

    const oldPlaneX = plane.x
    plane.x = plane.y
    plane.y = -oldPlaneX
  } else if (keys[LEFT_KEY]) {
    // Both camera direction and camera plane must be rotated
    const oldDirX = dir.x
    dir.x = -dir.y
    dir.y = oldDirX

    const oldPlaneX = plane.x
    plane.x = -plane.y
    plane.y = oldPlaneX
  }

  keys = {}

  context.fillStyle = 'black'
  context.fillRect(0, 0, canvasWidth, canvasHeight)

  for (let x = 0; x < canvasWidth; ++x) {
    // calculate ray position and direction
    const cameraX = 2 * x / canvasWidth - 1
    let rayPos = { x: pos.x, y: pos.y }
    let rayDir = { x: dir.x + plane.x * cameraX, y: dir.y + plane.y * cameraX }

    // which box of the map we're in
    const map = { x: Math.floor(rayPos.x), y: Math.floor(rayPos.y) }

    // length of ray from current position to next x or y-side
    const sideDist = { x: 0, y: 0 }

    // length of ray from one x or y-side to next x or y-side
    const deltaDist = {
      x: Math.sqrt(1 + (rayDir.y * rayDir.y) / (rayDir.x * rayDir.x)),
      y: Math.sqrt(1 + (rayDir.x * rayDir.x) / (rayDir.y * rayDir.y))
    }

    let perpWallDist = 0

    // what direction to step in x or y-direction (either +1 or -1)
    let step = { x: 0, y: 0 }

    // was a NS or a EW wall hit?
    let hit = false

    // was a NS or a EW wall hit?
    let side = false

    // calculate step and initial sideDist
    if (rayDir.x < 0) {
      step.x = -1
      sideDist.x = (rayPos.x - map.x) * deltaDist.x
    } else {
      step.x = 1
      sideDist.x = (map.x + 1.0 - rayPos.x) * deltaDist.x
    }

    if (rayDir.y < 0) {
      step.y = -1
      sideDist.y = (rayPos.y - map.y) * deltaDist.y
    } else {
      step.y = 1
      sideDist.y = (map.y + 1.0 - rayPos.y) * deltaDist.y
    }

    // Perform DDA
    while (!hit || oob(map)) {
      // Jump to next map square, OR in x-direction, OR in y-direction
      if (sideDist.x < sideDist.y) {
        sideDist.x += deltaDist.x
        map.x += step.x
        side = false
      } else {
        sideDist.y += deltaDist.y
        map.y += step.y
        side = true
      }

      // Check if ray has hit a wall
      hit = worldMap[map.x][map.y] > 0
    }

    if (oob(map)) {
      break
    }

    // Calculate distance projected on camera direction (oblique distance will give fisheye effect!)
    perpWallDist = side
      ? (map.y - rayPos.y + (1 - step.y) / 2) / rayDir.y
      : (map.x - rayPos.x + (1 - step.x) / 2) / rayDir.x

    // Calculate height of line to draw on screen
    const lineHeight = Math.floor(canvasHeight / perpWallDist)

    // Calculate lowest and highest pixel to fill in current stripe
    const drawStart = Math.max(0, (canvasHeight - lineHeight) / 2.0)
    const drawEnd = Math.min(canvasHeight - 1, (lineHeight + canvasHeight) / 2.0)

    // Calculate block color
    let color = {
      1: [255, 0, 0],
      2: [0, 0, 255],
      3: [0, 255, 0],
      4: [255, 255, 255]
    }[worldMap[map.x][map.y]] || [0, 255, 255]

    if (side) {
      color = color.map(c => Math.floor(c / 2))
    }

    // Draw ground
    context.beginPath()
    context.strokeStyle = 'brown'
    context.moveTo(x, canvasHeight - 1)
    context.lineTo(x, drawEnd)
    context.stroke()

    // Draw wall
    context.beginPath()
    context.strokeStyle = `rgb(${color[0]}, ${color[1]}, ${color[2]})`
    context.moveTo(x, drawStart)
    context.lineTo(x, drawEnd)
    context.stroke()

    // Draw ceiling
    context.beginPath()
    context.strokeStyle = 'gray'
    context.moveTo(x, drawStart)
    context.lineTo(x, 0)
    context.stroke()
  }

  window.requestAnimationFrame(draw)
}

window.requestAnimationFrame(draw)