#include <stdio.h>
#include <stdlib.h>
#include <iostream>

struct Node
{
	int data;
	Node *prev; // Pointer to next node
	Node *next; // Pointer to previous node
};


void insertEnd(Node** root, int value)
{
	if (*root == NULL)
	{
		Node* new_node = new Node();
		new_node->data = value;
		new_node->next = new_node->prev = new_node;
		*root = new_node;
		return;
	}
 

	Node *last = (*root)->prev;

	Node* new_node = new Node;
	new_node->data = value;

	new_node->next = *root;

	(*root)->prev = new_node;

	new_node->next = last;

	last->prev = new_node;
}

void display(Node* root)
{
	Node *temp = root;

	printf("\nİleriye doğru \n");
	while (temp->next != root)
	{
		printf("%d ", temp->data);
		temp = temp->next;
	}
	printf("%d ", temp->data);

	/*
	printf("\nGeriye doğru \n");
	Node *last = root->prev;
	temp = last;
	while (temp->prev != last)
	{
		printf("%d ", temp->data);
		temp = temp->prev;
	}
	printf("%d ", temp->data);
	*/
}

int main()
{
	Node* start = NULL;
	insertEnd(&start, 5);
	insertEnd(&start, 4);
	insertEnd(&start, 7);
	insertEnd(&start, 8);

	printf("Created circular doubly linked list is: ");
	display(start);

}