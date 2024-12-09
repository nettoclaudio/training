package main

import "testing"

func TestCrosswordSolver_CountMatches(t *testing.T) {
	tests := map[string]struct {
		board      [][]byte
		targetWord string
		expected   int
	}{
		"target word only on horizontal positions": {
			board: [][]byte{
				[]byte("XMAS..."),
				[]byte("...SAMX"),
				[]byte("XMASAMX"),
			},
			targetWord: "XMAS",
			expected:   4,
		},

		"target word only on vertical positions": {
			board: [][]byte{
				[]byte("X.....S"),
				[]byte("M.....A"),
				[]byte("A.....M"),
				[]byte("S.....X"),
				[]byte("A.....M"),
				[]byte("M.....A"),
				[]byte("X.....S"),
			},
			targetWord: "XMAS",
			expected:   4,
		},

		"target word on diagonals": {
			board: [][]byte{
				[]byte("S.....S"),
				[]byte(".A...A."),
				[]byte("..M.M.."),
				[]byte("...X..."),
				[]byte("..M.M.."),
				[]byte(".A...A."),
				[]byte("S.....S"),
			},
			targetWord: "XMAS",
			expected:   4,
		},

		"all directions": {
			board: [][]byte{
				[]byte("S..S..S"),
				[]byte(".A.A.A."),
				[]byte("..MMM.."),
				[]byte("SAMXMAS"),
				[]byte("..MMM.."),
				[]byte(".A.A.A."),
				[]byte("S..S..S"),
			},
			targetWord: "XMAS",
			expected:   8,
		},
	}

	for name, tt := range tests {
		t.Run(name, func(t *testing.T) {
			cc := &CrosswordSolver{Board: tt.board}

			matches := cc.CountMatches(tt.targetWord)
			if matches != tt.expected {
				t.Errorf("there are %d matches for %s but the expected is %d", matches, tt.targetWord, tt.expected)
			}
		})
	}
}

func TestCrosswordSolver_CountXMASes(t *testing.T) {
	tests := []struct {
		board    [][]byte
		expected int
	}{
		{
			board: [][]byte{
				[]byte("S.M"),
				[]byte(".A."),
				[]byte("S.M"),
			},
			expected: 1,
		},

		{
			board: [][]byte{
				[]byte(".M.S......"),
				[]byte("..A..MSMS."),
				[]byte(".M.S.MAA.."),
				[]byte("..A.ASMSM."),
				[]byte(".M.S.M...."),
				[]byte(".........."),
				[]byte("S.S.S.S.S."),
				[]byte(".A.A.A.A.."),
				[]byte("M.M.M.M.M."),
				[]byte(".........."),
			},
			expected: 9,
		},
	}

	for _, tt := range tests {
		t.Run("", func(t *testing.T) {
			cc := &CrosswordSolver{Board: tt.board}

			matches := cc.CountXMASes()
			if matches != tt.expected {
				t.Errorf("there are %d matches but the expected is %d", matches, tt.expected)
			}
		})
	}
}
