# OOXXwars

An advanced TicTacToe game based on Haskell and the Brick library.

## Who we are

| Name         | Email  | PID       |
| ------------ | ------ | --------- |
| Zhiyuan Qi   | zhqi   | A59002572 |
| Ruochen Wang | ruw028 | A59011298 |
| York Liu     | yil173 | A14689456 |
| Haofeng Xie  | haxie  | A59002570 |

## What it is

![](assets/demo.JPG)

This game is an advanced version of Tic-Tac-Toe. Note that, in a traditional Tic-Tac-Toe game, two players take turns to mark the spaces in a three-by-three grid with X or O. The player who succeeds in placing three of their marks in a horizontal, vertical, or diagonal row is the winner. 

Our game is a **nested** Tic-Tac-Toe game, which is composed of `3 x 3` traditional Tic-Tac-Toe games as its sub-games. Specifically, it is composed of a `3 x 3` big grid, and each grid in our game is a traditional Tic-Tac-Toe game. 

In our game, you and an minimax AI take turns to mark space in any grid in any sub-game with `X` or `O`, just like in traditional games. When a sub-game in one particular big grid is finished, the whole big grid will be marked with the mark used by the sub-game winner. And the mark of a big grid is determined by the win/loss result of the corresponding sub-Tic-Tac-Toe game. The player who succeeds in winning three sub-games horizontally, vertically, or diagonally is the winner.

Here is a list of controls available:

- `wasd`, `hjkl`, and arrow keys for movement
- `ESC` to end the game 

## Installation

This application is distributed using `stack`. Note that one of the audio-related dependencies `ALUT` is built upon the C library `libalut`. **This library needs to be installed to your system before you run `stack build`.**

To install `libalut` on Arch Linux:

```sh
sudo pacman -S freealut
```

To install `libalut` on Ubuntu:

```sh
sudo apt-get install -y libalut-dev
```

To install `libalut` on macOS:

```sh
brew install freealut
```

After this you're able to 

- clone this repository
- `stack run`: to run the game
- `stack run -- --help`: to show the game's help document
- `stack run <difficulty>`: to run the game with a certain difficulty (an integer from 0..10). The higher the number, the smarter the opposing AI. **Difficulty 10 means UNBEATABLE**.
- `stack test`: to run unit tests. 

## Code Architecture

We built upon the starter code. The following components are present in `src/`:

- `Model.hs`: defines the data types used in the game
- `SuperBoard.hs`: defines the superboard, which includes 9 boards, movements, and the winning conditions
- `Board.hs`: defines the subboard, which includes movements and the winning conditions of subboard
- `Player.hs`: implements two types of players, minimax AI and human
- `View.hs`: displays intro, the game itself, and outro
- `Control.hs`: defines movement  
- `Audio.hs`: play sounds upon movement, placement, win/loss. This is built upon the `Sound.ALUT` library. The sounds are from `mixkit.co`.

Also we have unit tests in `tests/`:

- `Tests.hs`: tests movement and winning conditions

## Teamwork

| Name         | Contribution                                     |
| ------------ | ------------------------------------------------ |
| Zhiyuan Qi   | Tests, SuperBoard, Control, data model           |
| Ruochen Wang | Intro/outro screen, Display of the board, Colors |
| York Liu     | Display of the board, Minimax AI, Audio          |
| Haofeng Xie  | Tests, SuperBoard, Control, data model           |

Timeline of this project:

- Nov 11 - Nov 17: Project Topic submission and confirmation.
- Nov 21 - Nov 28: First version (prototype done). Reached Milestone 2.
- Nov 30 - Dec 09: Final version and testing done. Reached Milestone 3.

## [Outdated, Milestone 1] Thoughts on Implementation

### Model

We’re going to structure the codebase based on the MVC framework. First of all, the essential data model of this game is a 3-d array `[[[ Entry ]]]` called *boards*, where 

- `Entry = O | X | Empty`
- `Boards[i]` is a 2D square board for the i-th subgame. 

### View

One section of the code is going to read this 3-d array and turn it into a TUI using the `Brick` Library. 

If time permits, we will also implement fancy ASCII art animations using Brick, such as when an entry is marked, when a subgame is finished, and when the game finishes.

### Controller

Another section of the code handles user input (keyboard), modification to the data model, and the winning logic. 

If time permits, we would also like to explore the option of mouse input.

### Extra

If everything above proceeds smoothly ahead of time, there are several other things we wish to try

- AI opponent (minimax, limit depth)
- Sound

## [Outdated, Milestone 1] Plan of execution

Two of us are going to handle the view component, and the other two will write the controller component. Below is a time line.

- Setup the basic data model and project folder structure by Sunday 11/14.
- Complete View and Controller by Sunday 11/21
- Present the updates by Sunday 11/25
- Write optional components by Sunday 12/5
- Deliver on Friday 12/10

## [Outdated, Milestone 2] Update

### What is the architecture of your application (the key components)?

We built upon the starter code, and currently we have the following components in `src`:
- `Model.hs`: defines the data types used in the game
- `SuperBoard.hs`: defines the superboard, which includes 9 boards, movements, and the winning conditions
- `Board.hs`: defines the subboard, which includes movements and the winning conditions of subboard
- `Player.hs`: adds strategy for superboard
- `View.hs`: displays the superboard with ASCII arts and color 
- `Control.hs`: supports superboard and 'hjkl' for movement

Also, we are going to implement the following components:
- `Audio.hs`: play sounds upon movement, placement, win/loss. We expect to use the `Sound.ALUT` library.
- update `Player.hs` to support the second player
- update `Player.hs` to support a minimax agent
- create a welcome page where user can choose between playing with an agent or a friend
- `Tests.hs`: add tests to movement and winning conditions using the QuickCheck library.

### What challenges (if any) did you have so far and how did you solve them?

- We encountered some difficulties in moving in superboard. In order to solve these issues, the teammates who developed the `View.hs` made the game display debugging information directly on the gameboard so that we could quickly pinpoint the issue. We finally fixed the bugs after collaboration.
- We found us unfamiliar with haskell documentation website. It took some time to get used to it.

### Do you expect to meet your goals until the deadline?

Currently we are optimistic about the project because we have finished the core components and the game is already working. We expect to meet our goals until the deadline.

### If not, how will you modify your goals?

N/A.
