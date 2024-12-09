package main

import (
	"errors"
	"fmt"
	"io"
	"strings"
)

func bool2int(t bool) int {
	if t {
		return 1
	}

	return 0
}

type CrosswordSolver struct {
	Board [][]byte
}

func (cc *CrosswordSolver) CountMatches(targetWord string) int {
	var matches int
	for i := 0; i < len(cc.Board); i++ {
		for j := 0; j < len(cc.Board[i]); j++ {
			matches += 0 +
				bool2int(cc.hasWord(targetWord, i, j, "RIGHT")) +
				bool2int(cc.hasWord(targetWord, i, j, "LEFT")) +
				bool2int(cc.hasWord(targetWord, i, j, "BOTTOM")) +
				bool2int(cc.hasWord(targetWord, i, j, "TOP")) +
				bool2int(cc.hasWord(targetWord, i, j, "TOP_LEFT")) +
				bool2int(cc.hasWord(targetWord, i, j, "TOP_RIGHT")) +
				bool2int(cc.hasWord(targetWord, i, j, "BOTTOM_LEFT")) +
				bool2int(cc.hasWord(targetWord, i, j, "BOTTOM_RIGHT"))
		}
	}

	return matches
}

func (cc *CrosswordSolver) hasWord(target string, i, j int, direction string) bool {
	if len(target) == 0 {
		return true
	}

	if (i < 0 || i >= len(cc.Board)) || (j < 0 || j >= len(cc.Board[i])) {
		return false
	}

	if cc.Board[i][j] != target[0] {
		return false
	}

	if strings.Contains(direction, "LEFT") {
		j--
	}

	if strings.Contains(direction, "RIGHT") {
		j++
	}

	if strings.Contains(direction, "TOP") {
		i--
	}

	if strings.Contains(direction, "BOTTOM") {
		i++
	}

	return cc.hasWord(target[1:], i, j, direction)
}

func (cc *CrosswordSolver) CountXMASes() int {
	var matches int
	for i := 0; i < len(cc.Board); i++ {
		for j := 0; j < len(cc.Board[i]); j++ {
			matches += bool2int(cc.isXMAS(i, j))
		}
	}

	return matches
}

func (cc *CrosswordSolver) isXMAS(i, j int) bool {
	if c := cc.Board[i][j]; c != 'A' {
		return false
	}

	if (i-1) < 0 || (j-1) < 0 {
		return false
	}

	if (i+1) >= len(cc.Board) || (j+1) >= len(cc.Board[i]) {
		return false
	}

	w1 := string([]byte{cc.Board[i-1][j-1], cc.Board[i][j], cc.Board[i+1][j+1]})
	w2 := string([]byte{cc.Board[i-1][j+1], cc.Board[i][j], cc.Board[i+1][j-1]})

	return (w1 == "MAS" || w1 == "SAM") && (w2 == "MAS" || w2 == "SAM")
}

func main() {
	var line []byte
	var board [][]byte

	for {
		var ch byte
		_, err := fmt.Scanf("%c", &ch)
		if errors.Is(err, io.EOF) {
			break
		}

		if ch == '\n' {
			board = append(board, line)
			line = make([]byte, 0, len(line))
			continue
		}

		line = append(line, ch)
	}

	cc := &CrosswordSolver{
		Board: board,
	}

	fmt.Println(
		"First puzzle answer is:",
		cc.CountMatches("XMAS"),
	)

	fmt.Println(
		"Second puzzle answer is:",
		cc.CountXMASes(),
	)
}
