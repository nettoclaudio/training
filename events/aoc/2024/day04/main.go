package main

import (
	"bufio"
	"fmt"
	"os"
)

func bool2int(t bool) int {
	if t {
		return 1
	}

	return 0
}

type CrosswordCracker struct {
	Board [][]byte
}

func (cc *CrosswordCracker) CountMatches(targetWord string) int {
	var matches int
	for i := 0; i < len(cc.Board); i++ {
		for j := 0; j < len(cc.Board[i]); j++ {
			matches += bool2int(cc.hasWordHorizontally(targetWord, i, j, false)) +
				bool2int(cc.hasWordHorizontally(targetWord, i, j, true)) +
				bool2int(cc.hasWordVertically(targetWord, i, j, false)) +
				bool2int(cc.hasWordVertically(targetWord, i, j, true)) +
				bool2int(cc.hasWordOnUpperLeftDiagonal(targetWord, i, j)) +
				bool2int(cc.hasWordOnUpperRightDiagonal(targetWord, i, j)) +
				bool2int(cc.hasWordOnBottomLeftDiagonal(targetWord, i, j)) +
				bool2int(cc.hasWordOnBottomRightDiagonal(targetWord, i, j))
		}
	}

	return matches
}

func (cc *CrosswordCracker) hasWordHorizontally(targetWord string, i, j int, reverse bool) bool {
	var found int
	for k := j; k >= 0 && k < len(cc.Board[i]); {
		if cc.Board[i][k] != targetWord[found] {
			return false
		}

		found += 1
		if found == len(targetWord) {
			return true
		}

		if reverse {
			k -= 1
		} else {
			k += 1
		}
	}

	return false
}

func (cc *CrosswordCracker) hasWordVertically(targetWord string, i, j int, reverse bool) bool {
	var found int
	for k := i; k >= 0 && k < len(cc.Board); {
		if cc.Board[k][j] != targetWord[found] {
			break
		}

		found += 1
		if found == len(targetWord) {
			return true
		}

		if reverse {
			k -= 1
		} else {
			k += 1
		}
	}

	return false
}

func (cc *CrosswordCracker) hasWordOnUpperLeftDiagonal(targetWord string, i, j int) bool {
	return cc.hasWordOnDiagonal(targetWord, i, j, true, true)
}

func (cc *CrosswordCracker) hasWordOnUpperRightDiagonal(targetWord string, i, j int) bool {
	return cc.hasWordOnDiagonal(targetWord, i, j, true, false)
}

func (cc *CrosswordCracker) hasWordOnBottomLeftDiagonal(targetWord string, i, j int) bool {
	return cc.hasWordOnDiagonal(targetWord, i, j, false, true)
}

func (cc *CrosswordCracker) hasWordOnBottomRightDiagonal(targetWord string, i, j int) bool {
	return cc.hasWordOnDiagonal(targetWord, i, j, false, false)
}

func (cc *CrosswordCracker) hasWordOnDiagonal(targetWord string, i, j int, reverseX, reverseY bool) bool {
	var found int

	for {
		x := i
		if reverseX {
			x -= found
		} else {
			x += found
		}

		if x < 0 || x >= len(cc.Board) {
			break
		}

		y := j
		if reverseY {
			y -= found
		} else {
			y += found
		}

		if y < 0 || y >= len(cc.Board[i]) {
			break
		}

		if cc.Board[x][y] != targetWord[found] {
			break
		}

		found += 1
		if found == len(targetWord) {
			return true
		}
	}

	return false
}

func main() {
	s := bufio.NewScanner(os.Stdin)
	s.Split(bufio.ScanLines)

	var board [][]byte

	for s.Scan() {
		board = append(board, s.Bytes())
	}

	cc := &CrosswordCracker{
		Board: board,
	}

	fmt.Println(
		"First puzzle answer is:",
		cc.CountMatches("XMAS"),
	)

	/*
		fmt.Println(
			"Second puzzle answer is:",
			123,
		)
	*/
}
