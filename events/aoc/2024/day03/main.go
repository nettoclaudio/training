package main

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
	"strconv"
)

type Instruction interface {
	Code() string
}

var _ Instruction = (*NoOpInstruction)(nil)

type NoOpInstruction struct {
	Name string
}

func (ins *NoOpInstruction) Code() string {
	return ins.Name
}

var _ Instruction = (*ArithmeticInstruction)(nil)

type ArithmeticInstruction struct {
	Operator string
	Left     int
	Right    int
}

func (ins *ArithmeticInstruction) Code() string {
	return ins.Operator
}

func (ins *ArithmeticInstruction) Result() int {
	switch ins.Operator {
	case "mul":
		return ins.Left * ins.Right

	default:
		return 0
	}
}

var instructionsFormat = regexp.MustCompile(`(mul|do|don\'t)\(((\d{1,3}),(\d{1,3}))?\)`)

func decodeInstrunctions(line string) []Instruction {
	var ins []Instruction

	matches := instructionsFormat.FindAllStringSubmatch(line, -1)
	for _, m := range matches {
		name := m[1]
		switch name {
		case "mul":
			n1, _ := strconv.Atoi(m[3])
			n2, _ := strconv.Atoi(m[4])

			ins = append(ins, &ArithmeticInstruction{Operator: name, Left: n1, Right: n2})

		default:
			ins = append(ins, &NoOpInstruction{Name: name})
		}
	}

	return ins
}

func SumAllMultiplications(insts []Instruction) int {
	var total int
	for _, ins := range insts {
		ains, ok := ins.(*ArithmeticInstruction)
		if !ok {
			continue
		}

		total += ains.Result()
	}

	return total
}

func SumAllMultiplicationsWithConditionals(insts []Instruction) int {
	var total int
	var disabled bool

	for _, ins := range insts {
		switch ins.Code() {
		case "do":
			disabled = false

		case "don't":
			disabled = true

		case "mul":
			if disabled {
				continue
			}

			total += ins.(*ArithmeticInstruction).Result()
		}
	}

	return total
}

func main() {
	s := bufio.NewScanner(os.Stdin)
	s.Split(bufio.ScanLines)

	var inst []Instruction

	for s.Scan() {
		inst = append(inst, decodeInstrunctions(s.Text())...)
	}

	fmt.Println(
		"First puzzle answer is:",
		SumAllMultiplications(inst),
	)

	fmt.Println(
		"Second puzzle answer is:",
		SumAllMultiplicationsWithConditionals(inst),
	)
}
