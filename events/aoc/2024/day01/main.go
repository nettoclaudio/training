package main

import (
	"errors"
	"fmt"
	"io"
	"math"
	"strings"
)

type LinkedListNode struct {
	Next  *LinkedListNode
	Value int
}

func NewLinkedListNode(n int) *LinkedListNode {
	return &LinkedListNode{Value: n}
}

func (node *LinkedListNode) String() string {
	var sb strings.Builder

	for current := node; current != nil; current = current.Next {
		fmt.Fprintf(&sb, "%d, ", current.Value)
	}

	sb.WriteString("nil")
	return sb.String()
}

func (node *LinkedListNode) SortedInsert(n int) *LinkedListNode {
	if node == nil || node.Value > n {
		return &LinkedListNode{Value: n, Next: node}
	}

	for current := node; current != nil; current = current.Next {
		if current.Next == nil || current.Next.Value > n {
			current.Next = &LinkedListNode{Value: n, Next: current.Next}
			break
		}
	}

	return node
}

type SortedLinkedList struct {
	Head   *LinkedListNode
	Length int
}

func (ll *SortedLinkedList) Insert(n int) *SortedLinkedList {
	if ll == nil {
		return &SortedLinkedList{Head: NewLinkedListNode(n), Length: 1}
	}

	ll.Head = ll.Head.SortedInsert(n)
	ll.Length += 1

	return ll
}

func (ll *SortedLinkedList) OccurrencesOf(n int) (total int) {
	if ll == nil {
		return
	}

	for head := ll.Head; head != nil; head = head.Next {
		if head.Value < n {
			continue
		}

		if head.Value > n {
			break
		}

		total += 1
	}

	return
}

func SumByArrayItemDistance(a, b *SortedLinkedList) int {
	var total int

	for h1, h2 := a.Head, b.Head; h1 != nil || h2 != nil; h1, h2 = h1.Next, h2.Next {
		total += int(math.Abs(float64(h1.Value - h2.Value)))
	}

	return total
}

func SumBySimilarityScore(left, right *SortedLinkedList) int {
	ocurrences := make(map[int]int)

	for head := left.Head; head != nil; head = head.Next {
		ocurrences[head.Value] = right.OccurrencesOf(head.Value)
	}

	var total int
	for n, count := range ocurrences {
		total += n * count
	}

	return total
}

func main() {
	var left, right *SortedLinkedList

	for {
		var n1, n2 int
		_, err := fmt.Scanf("%d %d", &n1, &n2)
		if errors.Is(err, io.EOF) {
			break
		}

		left, right = left.Insert(n1), right.Insert(n2)
	}

	fmt.Println(
		"First puzzle answer is:",
		SumByArrayItemDistance(left, right),
	)

	fmt.Println(
		"Second puzzle answer is:",
		SumBySimilarityScore(left, right),
	)
}
