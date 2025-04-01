package org.yashnova;

import org.junit.Assert;
import org.junit.Test;

import java.util.*;

import static org.junit.Assert.*;

public class MyLinkedListTest {

    @Test
    public void testAdd_EmptyList() {
        MyLinkedList<Integer> list = new MyLinkedList<>();
        list.add(1);
        Assert.assertEquals(1, list.size());
    }

    @Test
    public void testSingleton() {
        MyLinkedList<Integer> lstInt = new MyLinkedList<>();
        lstInt.add(2);
        Assert.assertEquals(new Integer(2), lstInt.get(0));
        Assert.assertEquals(1, lstInt.size());
        lstInt.remove();
        assertTrue(lstInt.isEmpty());


        MyLinkedList<String> lstStr = new MyLinkedList<>();
        lstStr.add("Arseniy");
        Assert.assertEquals("Arseniy", lstStr.get(0));
        lstStr.remove(0);
        assertTrue(lstStr.isEmpty());

        lstStr.add("Arseniy");
        Assert.assertEquals("Arseniy", lstStr.get(0));
        lstStr.set(0, "Gogo");
        Assert.assertEquals("Gogo", lstStr.get(0));
    }

    @Test
    public void testAddRemoveDbl() {
        MyLinkedList<Double> lstDbl = new MyLinkedList<>();
        for (int i = 0; i < 10; i++) {
            lstDbl.add(((double) i) / 2);
        }

        Assert.assertEquals(10, lstDbl.size());
        Double[] curArr = lstDbl.toArray(new Double[0]);
        Double[] testArr = {0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5};
        assertArrayEquals(testArr, curArr);

        for (int i = 0; i < 10; i++) {
            Assert.assertEquals(new Double(((double) i) / 2), lstDbl.get(i));
        }
        lstDbl.set(2, 9.0);
        Assert.assertEquals(new Double(9.0), lstDbl.get(2));
        lstDbl.remove(2);
        Assert.assertEquals(new Double(1.5), lstDbl.get(2));
        Assert.assertEquals(new Double(0.5), lstDbl.get(1));
        Assert.assertEquals(9, lstDbl.size());
        int n = lstDbl.size();
        for (int i = 0; i < n; i++) {
            lstDbl.remove();
        }

        assertTrue(lstDbl.isEmpty());

    }

    @Test
    public void testAddRemMore() {
        MyLinkedList<Double> lstDbl = new MyLinkedList<>();

        for (int i = 0; i < 100; i++) {
            lstDbl.add(((double) i) / 2);
        }
        for (int i = 0; i < 100; i++) {
            Assert.assertEquals(new Double(((double) i) / 2), lstDbl.get(i));
        }
        Assert.assertEquals(100, lstDbl.size());
    }

    @Test
    public void testIterSetGet() {
        MyLinkedList<String> list = new MyLinkedList<>();
        list.add("A");
        list.add("X");
        list.add("C");

        // Create a ListIterator starting at index 1
        MyLinkedList.MyListIterator iterator = (MyLinkedList.MyListIterator) list.listIterator(1);
        iterator.set("X");
        Assert.assertEquals("X", list.get(1));
    }

    @Test
    public void testIterNextPrev() {
        MyLinkedList<String> list = new MyLinkedList<>();
        list.add("A");
        list.add("X");
        list.add("C");

        // Create a ListIterator starting at index 1
        MyLinkedList.MyListIterator iterator = (MyLinkedList.MyListIterator) list.listIterator(0);
        LinkedList<String> oList = new LinkedList<>();
        oList.add("A");
        oList.add("X");
        oList.add("C");
        ListIterator oIter = oList.listIterator(0);

        assertEquals(iterator.next(), oIter.next());
        assertEquals(iterator.next(), oIter.next());
        assertEquals(iterator.previous(), oIter.previous());
        assertEquals(iterator.previous(), oIter.previous());
    }

    @Test
    public void testIterSingle() {
        MyLinkedList<Double> singleD = new MyLinkedList<>();
        singleD.add(0.4);
        MyLinkedList.MyListIterator iterD = (MyLinkedList.MyListIterator) singleD.listIterator();
        assertEquals(false, iterD.hasNext());
        assertEquals(false, iterD.hasPrevious());
    }

    @Test
    public void testIterIndex() {
        MyLinkedList<Integer> linkedList = new MyLinkedList<>();
        linkedList.add(5);
        linkedList.add(6);
        linkedList.add(7);
        linkedList.add(8);

        MyLinkedList.MyListIterator intIter = (MyLinkedList.MyListIterator) linkedList.listIterator(1);
        assertEquals(5, intIter.previous());
        assertEquals(1, intIter.nextIndex());
        intIter.next();
        assertEquals(0, intIter.previousIndex());
        intIter.next();
        assertEquals(2, intIter.nextIndex());
    }

    @Test
    public void testAddAll() {
        MyLinkedList<Integer> newIntList = new MyLinkedList<>();
        Collection<Integer> toAdd = Arrays.asList(1, 2, 3, 4);

        assertTrue(newIntList.addAll(toAdd));
        for (int i = 1; i < newIntList.size() + 1; i++) {
            Assert.assertEquals(new Integer(i), newIntList.get(i - 1));
        }
    }

    @Test
    public void testAddAllWithIndex() {
        MyLinkedList<Integer> newIntList = new MyLinkedList<>();
        Collection<Integer> toAdd = Arrays.asList(1, 2, 3, 4);
        newIntList.add(2);

        assertTrue(newIntList.addAll(1, toAdd));

        for (int i = 1; i < newIntList.size(); i++) {
            Assert.assertEquals(new Integer(i), newIntList.get(i));
        }
    }

    @Test
    public void testRemoveAll() {
        MyLinkedList<Integer> newIntList = new MyLinkedList<>();
        LinkedList<Integer> newIntList2 = new LinkedList<>();

        Collection<Integer> toAdd = Arrays.asList(1, 2, 3, 4);
        newIntList.addAll(toAdd);
        newIntList2.addAll(toAdd);

        Collection<Integer> toRemove = Arrays.asList(2, 4);

        assertTrue(newIntList.removeAll(toRemove));
        newIntList2.removeAll(toRemove);
        Assert.assertEquals(newIntList2.get(0), newIntList.get(0));
        Assert.assertEquals(newIntList2.get(1), newIntList.get(1));
        Assert.assertEquals(newIntList2.size(), newIntList.size());

    }

    @Test
    public void testRetainAll() {
        MyLinkedList<String> newStrList = new MyLinkedList<>();
        LinkedList<String> newStrList2 = new LinkedList<>();

        newStrList.add("2");
        newStrList.add("3");
        newStrList2.add("2");
        newStrList2.add("3");

        Collection<String> toRet = Arrays.asList("1", "2", "4");


        assertTrue(newStrList.retainAll(toRet));
        newStrList2.retainAll(toRet);
        Assert.assertEquals(newStrList2.get(0), newStrList.get(0));
        Assert.assertEquals(newStrList2.size(), newStrList.size());

    }

    @Test
    public void testCompareList_MyList() {
        MyLinkedList<Integer> myList = new MyLinkedList<>();
        LinkedList<Integer> javaList = new LinkedList<>();

        // Add elements to both lists
        List<Integer> elements = Arrays.asList(1, 2, 3, 4, 5);
        myList.addAll(elements);
        javaList.addAll(elements);

        // Test basic operations (add, get, size)
        Assert.assertEquals(myList.size(), javaList.size());
        for (int i = 0; i < myList.size(); i++) {
            Assert.assertEquals(myList.get(i), javaList.get(i));
        }

    }

    @Test
    public void testAddAftrRemove() {
        MyLinkedList<Integer> tmp = new MyLinkedList<>();
        tmp.add(3);
        tmp.remove();

        Assert.assertEquals(0, tmp.size());
        tmp.add(3);
        Assert.assertEquals(1, tmp.size());
    }

    @Test
    public void testIndexOf() {
        MyLinkedList<String> strList = new MyLinkedList<>();
        strList.add("aa");
        strList.add("bb");
        strList.add("a");
        strList.add("bbb");
        strList.add("bb");
        LinkedList<String> strList2 = new LinkedList<>();
        strList2.add("aa");
        strList2.add("bb");
        strList2.add("a");
        strList2.add("bbb");
        strList2.add("bb");
        Assert.assertEquals(strList2.indexOf("aa"), strList.indexOf("aa"));
        Assert.assertEquals(strList2.indexOf("bb"), strList.indexOf("bb"));
        Assert.assertEquals(strList2.indexOf("a"), strList.indexOf("a"));
    }

    @Test
    public void testLastIndexOf() {
        MyLinkedList<String> strList = new MyLinkedList<>();
        strList.add("aa");
        strList.add("bb");
        strList.add("a");
        strList.add("bbb");
        strList.add("bb");
        MyLinkedList<String> strList2 = new MyLinkedList<>();
        strList2.add("aa");
        strList2.add("bb");
        strList2.add("a");
        strList2.add("bbb");
        strList2.add("bb");
        Assert.assertEquals(strList2.lastIndexOf("aa"), strList.lastIndexOf("aa"));
        Assert.assertEquals(strList2.lastIndexOf("bb"), strList.lastIndexOf("bb"));
        Assert.assertEquals(strList2.lastIndexOf("a"), strList.lastIndexOf("a"));
    }

    @Test
    public void testAdd() {
        MyLinkedList<String> strList = new MyLinkedList<>();
        strList.add("aa");
        strList.add("bb");
        strList.add("a");
        strList.add("bbb");
        strList.add("bb");
        LinkedList<String> strList2 = new LinkedList<>();
        strList2.add("aa");
        strList2.add("bb");
        strList2.add("a");
        strList2.add("bbb");
        strList2.add("bb");
        for (int i = 0; i < strList2.size(); i++) {
            Assert.assertEquals(strList2.get(i), strList.get(i));

        }
    }

    @Test
    public void testGet() {
        MyLinkedList<Integer> strList = new MyLinkedList<>();
        strList.add(2);
        strList.add(1);

        LinkedList<Integer> strList2 = new LinkedList<>();
        strList2.add(2);
        strList2.add(1);

        for (int i = 0; i < strList2.size(); i++) {
            Assert.assertEquals(strList2.get(i), strList.get(i));

        }
    }

    @Test
    public void testAddIndex() {
        MyLinkedList<Integer> strList = new MyLinkedList<>();
        strList.add(2);
        strList.add(1);

        LinkedList<Integer> strList2 = new LinkedList<>();
        strList2.add(2);
        strList2.add(1);
        strList2.add(1, 6);
        strList.add(1, 6);
        for (int i = 0; i < strList.size(); i++) {
            Assert.assertEquals(strList2.get(i), strList.get(i));

        }
    }

    @Test
    public void testClear() {
        MyLinkedList<Integer> strList = new MyLinkedList<>();
        strList.add(2);
        strList.add(1);

        LinkedList<Integer> strList2 = new LinkedList<>();
        strList2.add(2);
        strList2.add(1);
        strList2.clear();
        strList.clear();
        Assert.assertEquals(strList2.isEmpty(), strList.isEmpty());
    }

    @Test
    public void testContains() {
        MyLinkedList<Integer> strList = new MyLinkedList<>();
        strList.add(2);
        strList.add(1);

        LinkedList<Integer> strList2 = new LinkedList<>();
        strList2.add(2);
        strList2.add(1);

        Assert.assertEquals(strList2.contains(2), strList.contains(2));
        Assert.assertEquals(strList2.contains(4), strList.contains(4));

    }

    @Test
    public void testContainsAll() {
        MyLinkedList<Integer> newIntList = new MyLinkedList<>();
        Collection<Integer> toAdd = Arrays.asList(0, 1, 2, 3);
        assertTrue(newIntList.addAll(toAdd));

        for (int i = 1; i < newIntList.size(); i++) {
            assertTrue(newIntList.contains(i));
        }
    }

    @Test
    public void testSet() {
        MyLinkedList<Integer> newIntList = new MyLinkedList<>();

        Collection<Integer> toAdd = Arrays.asList(0, 1, 2, 3);
        assertTrue(newIntList.addAll(toAdd));

        LinkedList<Integer> newIntList2 = new LinkedList<>(toAdd);
        newIntList2.set(0,44);
        newIntList.set(0,44);
        Assert.assertEquals(newIntList2.get(0), newIntList.get(0));
    }




}
