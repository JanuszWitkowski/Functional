/*
Wprawki w programowanieu w JavaScript
*/


// do ćwiczenia
function myFnc(x,y) {
    return x*(x+y);
}

// JS nie ma list jako typu natywnego
// modelować je będziemy jako tablice

function isEmpty(arr){
    return arr.length==0
}

//pierwszy element listy
function head(arr) {
    return arr[0];
}

//Ogon listy
function tail(arr) {
    return arr.slice(1)
}

//rekurencyjna wersja sumy

function mySum(arr) {
    if (isEmpty(arr)) {return 0;}
    else {return head(arr) + mySum(tail(arr));}
}

// ZADANIE: przerób mySum na wersję indukcji ogonowej     

function mySum2(arr, acc) {
    if (isEmpty(arr)) { return acc; }
    else { return mySum2(tail(arr), acc + head(arr)); }
}


// ZADANIE 8
function fnc8(x, y) {
    return (z => (w => z + 2*w))(x * y);
}

function fnc8_v2(x, y) {
    return (w => x * y + 2 * w);
}


// ZADANIE 11
function sumOfList(arr) {
    if (isEmpty(arr)) return 0;
    return head(arr) + sumOfList(tail(arr));
}

function productOfList(arr) {
    if (isEmpty(arr)) return 1;
    return head(arr) * productOfList(tail(arr));
}

function minFromList(arr, mini) {
    if (isEmpty(arr)) return mini;
    if (head(arr) < mini) {
        return minFromList(tail(arr), head(arr));
    }
    return minFromList(tail(arr), mini);
}

function maxFromList(arr, maxi) {
    if (isEmpty(arr)) return maxi;
    if (head(arr) > maxi) {
        return maxFromList(tail(arr), head(arr));
    }
    return maxFromList(tail(arr), maxi);
}


let array = [3, 1, 4, 2];
console.log(mySum(array))
console.log(mySum2(array, 0))
console.log(fnc8(2, 3)(6))
console.log(fnc8_v2(2, 3)(6))
console.log(sumOfList(array))
console.log(productOfList(array))
console.log(minFromList(array, 123))
console.log(maxFromList(array, -123))
console.log(array)
