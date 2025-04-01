package org.yashnova;

import java.util.*;


public class MyLinkedList<T> implements List<T> {
    Node<T> head;
    Node<T> tail;

    MyLinkedList() {
        this.head = null;
        this.tail = null;
    }

    @Override
    public Iterator<T> iterator() {
        return (Iterator<T>) new MyListIterator(head);
    }

    @Override
    public Object[] toArray() {
        Object[] array = new Object[this.size()];
        int i = 0;
        Node<T> current = head;
        while (current != null) {
            array[i++] = current.data;
            current = current.next;
        }
        return array;
    }

    @Override
    public <T1> T1[] toArray(T1[] a) {
        if (a.length < this.size()) {
            a = (T1[]) Arrays.copyOf(a, this.size());
        }
        int i = 0;
        Node<T> current = head;
        while (current != null) {
            a[i++] = (T1) current.data;
            current = current.next;
        }
        if (a.length > this.size()) {
            a[this.size()] = null;
        }
        return a;
    }


    class MyListIterator implements ListIterator<T> {

        Node<T> current;

        public MyListIterator(Node first) {
            current = first;
        }


        @Override
        public boolean hasNext() {
            return current.next != null;
        }

        @Override
        public T next() {
            Node<T> tempo = current;
            current = current.next;
            return tempo.data;
        }

        @Override
        public boolean hasPrevious() {
            return current.prev != null;
        }

        @Override
        public T previous() {
            Node<T> tempo = current;
            current = current.prev;
            return current.data;
        }

        @Override
        public int nextIndex() {
            if (hasNext()) {
                int i = 0;
                while (hasPrevious()) {
                    previous();
                    i++;
                }
                return i + 1;
            } else return -1;
        }

        @Override
        public int previousIndex() {
            if (hasPrevious()) {
                int i = 0;
                while (hasPrevious()) {
                    previous();
                    i++;
                }
                return i - 1;
            } else return -1;
        }

        @Override
        public void remove() {
            if (current == null) {
                throw new IllegalStateException("Cannot remove: iterator is not pointing to a valid element");
            }

            // If we are at the head of the list
            if (current.prev == null) {
                current = current.next;
                if (current != null) {
                    current.prev = null;
                }
            } else if (current.next == null) { // If we are at the tail
                current = current.prev;
                current.next = null;
            } else { // If we are in the middle of the list
                Node prev = current.prev;
                Node next = current.next;
                prev.next = next;
                next.prev = prev;
                current = next;
            }
        }

        @Override
        public void set(T t) {
            if (current == null) {
                throw new IllegalStateException("Cannot set: iterator is not pointing to a valid element");
            }
            current.data = t; // Assuming Node has a 'data' field to be replaced
        }

        @Override
        public void add(T t) {
            Node node = new Node<>(t);
            if (current == null) { // Adding at the head of the list
                node.next = current;
                current = node;
            } else { // Adding in the middle or tail
                Node next = current.next;
                current.next = node;
                node.prev = current;
                node.next = next;
                if (next != null) {
                    next.prev = node;
                }
                current = node;
            }

        }

    }

    static class Node<T> {
        T data;
        Node<T> next;
        Node<T> prev;

        Node(T data) {
            this.data = data;
            this.prev = null;
            this.next = null;
        }
    }

    public int size() {
        int count = 0;
        Node current = head; // Предполагается, что у вас есть ссылка на голову списка

        while (current != null) {
            count++;
            current = current.next; // Переходим к следующему узлу
        }

        return count; // Возвращаем общее количество узлов
    }

    public boolean contains(Object data) {
        Node<T> cur = head;
        while (cur != null) {
            if (cur.data == data) {
                return true;
            }
            cur = cur.next;
        }
        return false;
    }

    public boolean add(T data) {//добавляет в конец
        Node<T> temp = new Node<>(data);
        if (head == null) {
            head = temp;
            tail = temp;
        } else {
            temp.prev = tail;
            tail.next = temp;
            tail = temp;
        }
        return true;
    }

    private void removeNode(Node<T> n) {
        if (this.size() == 1) {
            head = null;
            tail = null;
            return;
        }
        Node<T> next = n.next;
        Node<T> prev = n.prev;

        if (prev != null) {
            prev.next = next;
        } else {
            head = next;
        }

        if (next != null) {
            next.prev = prev;
        } else {
            tail = prev;
        }
    }

    @Override
    public boolean remove(Object o) {//удаляет первое значение
        if (this.find(o) == null) {
            return false;
        }
        Node<T> n = find(o);
        removeNode(n);
        return true;
    }

    @Override
    public boolean containsAll(Collection<?> c) {
        Node<T> cur = head;
        for (var elem : c) {
            if (!this.contains(elem)) {
                return false;
            }
        }
        return true;
    }

    @Override
    public boolean addAll(Collection<? extends T> c) {
        Node<T> current = tail; // Start at the tail node

        for (var elem : c) {
            Node<T> newNode = new Node<>(elem); // Create a new node for the element

            if (current == null) { // If the list is empty
                head = newNode;
                tail = newNode;
            } else {
                current.next = newNode; // Link the new node to the current node
                newNode.prev = current; // Link the current node to the new node
                tail = newNode; // Update tail if necessary
            }
            current = newNode; // Move to the newly added node
        }
        return true;
    }

    @Override
    public boolean addAll(int index, Collection<? extends T> c) {
        if (index < 0 || index > this.size()) {
            throw new IndexOutOfBoundsException("Wrong index: " + index);
        }
        // Создаем копию коллекции c
        List<T> copy = new ArrayList<>(c);
        int cur_ind = index;
        for (T elem : copy) {
            this.add(cur_ind, elem);
            cur_ind++;
        }
        return true;
    }
    @Override
    public boolean removeAll(Collection<?> c) {
        for (var elem : c) {
            while (contains(elem)) {
                removeNode(find(elem));
            }
        }
        return true;
    }

    @Override
    public boolean retainAll(Collection<?> c) {
        boolean changed = false;
        Node<T> cur = head;
        while (cur != null) {
            if (!c.contains(cur.data)) {
                Node<T> tmp = cur.next;
                this.removeNode(cur);
                cur = tmp;
                changed = true;
            } else {
                cur = cur.next;
            }
        }
        return changed;
    }


    public void clear() {
        while (!isEmpty()) {
            removeNode(this.head); // Удаляем узел из начала списка
        }
        head = null; // Устанавливаем head в null
        tail = null; // Устанавливаем tail в null
    }

    public void add(int ind, T data) throws IndexOutOfBoundsException {
        if (ind < 0 || ind > this.size()) {
            throw new IndexOutOfBoundsException("Wrong index: " + ind);
        }

        Node<T> newNode = new Node<>(data);

        // Добавление в начало списка
        if (ind == 0) {
            if (head == null) { // Если список пуст
                head = newNode;
                tail = newNode; // Указываем, что хвост тоже новый узел
            } else {
                newNode.next = head;
                head.prev = newNode;
                head = newNode;
            }
            return;
        }

        // Добавление в конец списка
        if (ind == this.size()) {
            if (tail == null) { // Если список пуст
                head = newNode;
                tail = newNode; // Указываем, что хвост тоже новый узел
            } else {
                tail.next = newNode;
                newNode.prev = tail;
                tail = newNode;
            }
            return;
        }

        // Добавление в середину списка
        Node<T> current = head;
        for (int i = 0; i < ind; i++) {
            current = current.next;
        }

        Node<T> previous = current.prev;

        previous.next = newNode;
        newNode.prev = previous;
        newNode.next = current;
        current.prev = newNode;

    }


    public boolean isEmpty() {
        return this.size() == 0;
    }

    private Node<T> find(Object data) {
        Node<T> cur = head;
        while (cur != null) {
            if (cur.data == data) {
                return cur;
            }
            cur = cur.next;
        }
        return null;
    }

    private Node<T> getInd(int ind) {
        if (ind < 0 || ind > this.size()) {
            throw new IndexOutOfBoundsException("Wrong index: " + ind);
        }
        Node<T> cur = head;
        while (ind > 0) {
            cur = cur.next;
            ind--;
        }
        return cur;
    }

    public T remove() {

        T data = this.head.data;
        head.prev = null;
        head = head.next;

        return data;
    }

    public T remove(int ind) throws IndexOutOfBoundsException {
        if (ind < 0 || ind > this.size()) {
            throw new IndexOutOfBoundsException("Wrong index: " + ind);
        }
        if (ind == 0) {
            return this.remove();
        }
        Node<T> cur = this.getInd(ind);
        Node<T> prev = cur.prev;
        Node<T> next = cur.next;
        prev.next = next;
        next.prev = prev;
        return cur.data;
    }

    @Override
    public int indexOf(Object o) {
        int ind = 0;
        Node<T> cur = head;
        while (cur != null) {
            if (o == cur.data) {
                return ind;
            }
            ind++;
            cur = cur.next;
        }
        return -1;
    }

    @Override
    public int lastIndexOf(Object o) {
        int ind = 0;
        Node<T> cur = tail;
        while (cur != null) {
            if (o == cur.data) {
                return this.size() - ind - 1;
            }
            ind++;
            cur = cur.prev;
        }
        return -1;
    }

    @Override
    public ListIterator<T> listIterator() {
        return new MyListIterator(head);
    }

    @Override
    public ListIterator<T> listIterator(int index) {
        return new MyListIterator(getInd(index));
    }

    @Override
    public List<T> subList(int fromIndex, int toIndex) {
        return null;
    }


    public T get(int ind) {
        if (ind < 0 || ind > this.size()) {
            throw new IndexOutOfBoundsException("Wrong index: " + ind);
        }
        return this.getInd(ind).data;
    }

    public T set(int ind, T data) {
        if (ind < 0 || ind > this.size()) {
            throw new IndexOutOfBoundsException("Wrong index: " + ind);
        }
        Node<T> cur = this.getInd(ind);
        cur.data = data;
        return data;
    }


}