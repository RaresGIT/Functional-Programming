test_array = [1,3,12,553,456,21,4,67,5]

def merge(a1, a2):
  if len(a1) == 0:
    return a2
  if len(a2) == 0:
    return a1
  if a1[0] <= a2[0]:
    rec = merge(a1[1:], a2)
    return [a1[0]] + rec
  rec = merge(a1, a2[1:])
  return [a2[0]] + rec

def merge_sort(arr):
  if len(arr) <= 1:
    return arr
  halfway = len(arr) // 2
  left = merge_sort(arr[:halfway])
  right = merge_sort(arr[halfway:])
  return merge(left, right)


print(merge_sort(test_array))