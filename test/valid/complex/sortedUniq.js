// from lodash
function sortedUniq(array, iteratee) {
    var seen,
        index = -1,
        length = array.length,
        resIndex = -1,
        result = [];

    while (++index < length) {
        var value = array[index],
            computed = iteratee(value, index, array);

        if (index === 0 || seen !== computed) {
            seen = computed;
            result[++resIndex] = value;
        }
    }
    return result;
}
