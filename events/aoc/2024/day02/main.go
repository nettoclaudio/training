package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
	"slices"
	"strconv"
	"strings"
)

func abs(n int) int {
	return int(math.Abs(float64(n)))
}

func removeElementAt(src []int, index int) []int {
	ll := slices.Clone(src)
	return append(ll[:index], ll[index+1:]...)
}

func isMovimentSafe(current, next int, increasing bool) bool {
	distance := current - next
	if d := abs(distance); d < 1 || d > 3 {
		return false
	}

	return increasing == (distance > 0)
}

func isSafe(levels []int) bool {
	increasing := (levels[0] - levels[1]) > 0

	for i := 0; i+1 < len(levels); i++ {
		if !isMovimentSafe(levels[i], levels[i+1], increasing) {
			return false
		}
	}

	return true
}

func isSafeReportByProblemDampener(levels []int) bool {
	if isSafe(levels) {
		return true
	}

	for j := 0; j < len(levels); j++ {
		if ll := removeElementAt(slices.Clone(levels), j); isSafe(ll) {
			return true
		}
	}

	return false
}

func CountSafeReports(reports [][]int) int {
	var total int
	for _, levels := range reports {
		if !isSafe(levels) {
			continue
		}

		total += 1
	}

	return total
}

func CountSafeReportsUsingProblemDampener(reports [][]int) int {
	var total int
	for _, levels := range reports {
		if !isSafeReportByProblemDampener(levels) {
			continue
		}

		total += 1
	}

	return total
}

func main() {
	s := bufio.NewScanner(os.Stdin)
	s.Split(bufio.ScanLines)

	var reports [][]int

	for i := 0; s.Scan(); i++ {
		var levels []int
		for _, col := range strings.Split(s.Text(), " ") {
			n, _ := strconv.Atoi(col)
			levels = append(levels, n)
		}

		reports = append(reports, levels)
	}

	fmt.Println(
		"First puzzle answer is:",
		CountSafeReports(reports),
	)

	fmt.Println(
		"Second puzzle answer is:",
		CountSafeReportsUsingProblemDampener(reports),
	)
}
