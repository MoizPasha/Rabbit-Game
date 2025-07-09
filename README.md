# 🐇 Jumping Rabbit Game (Intel 8088 Assembly)

A console-based animation game *Jumping Rabbit* developed in **Intel 8088 Assembly**. The game features animated scenery, a jumping rabbit controlled by the user, dynamic bricks, collectible carrots, and real-time scoring — all implemented using low-level assembly with keyboard and timer interrupt handling.

---

## 🛠️ Technology Stack

- **Platform**: DOS (8088 real mode)
- **Language**: Assembly (MASM/TASM syntax)
- **Graphics**: Text-mode console (80x25)
- **Interrupts**:
  - for keyboard input
  - for game timing and updates

---

## 🎮 Game Features

- **Scrolling Scene**:
  - Top section: Background scenery (e.g., trees, mountains) scrolling rightward
  - Middle section: Foreground (e.g., road or sea) scrolling leftward
  - Bottom section: Reserved for gameplay area

- **Player Mechanics**:
  - Jumping rabbit controlled with the **Up arrow key**
  - Game over if the rabbit jumps into empty space (misses a brick)

- **Platform System**:
  - Moving colored bricks (yellow, orange, blue) act as platforms
  - Brick color and placement are randomized
  - Blue bricks break on impact

- **Scoring**:
  - Carrots appear on some bricks
  - Rabbit collects carrots to increase score
  - Score is displayed on the screen
  - Brick movement speed increases with score

---

## 🎮 Controls

| Key         | Function      |
|-------------|---------------|
| ↑ Arrow     | Jump          |

---

## 🧾 Running the Game

### Requirements:
- DOSBox or compatible x86 emulator

