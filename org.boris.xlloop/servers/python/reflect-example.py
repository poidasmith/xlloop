
import xlloop

# All arguments to expose are optional. Could use '@expose()' instead.
@xlloop.expose(
excel_name='py_sum',
arg_descrs=['arr is a range of numbers to sum']
)
def test_sum(arr):
    'Returns the summation of arr'
    return sum(arr)

xlloop.run(5460)