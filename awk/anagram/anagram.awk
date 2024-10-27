# These variables are initialized on the command line (using '-v'):
# - key

BEGIN { normalized_key = sortw(key) }
tolower($0) != tolower(key) && is_anagram($0) { print } # print normalized_key ";" sortw($0) }

function is_anagram(word) {
    return sortw(word) == normalized_key
}

function sortw(word,        result, arr) {
    split(tolower(word), arr, "")
    asort(arr)
    for (i in arr) {
        result = result arr[i]
    }
    return result
}
