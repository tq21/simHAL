library(reticulate)

py_run_string("
import numpy as np

class BasisMaker:
    def __init__(self, X):
        self.X = X
        self.knots = X

    def _basis_products(self, arr, index=0, current=None, result=None):
        if result is None:
            result = []
        if current is None:
            current = np.ones_like(arr[0], dtype=bool)

        if index == len(arr):
            result.append(current)
        else:
            self._basis_products(arr, index + 1, current & arr[index], result)
            self._basis_products(arr, index + 1, current, result)

        return result

    def _bases(self, X):
        one_way_bases = np.stack([
            np.less_equal.outer(self.knots[:, j], X[:, j])
            for j in range(self.knots.shape[1])
        ])
        bases = self._basis_products(one_way_bases)
        return np.concatenate(bases[:-1]).T

    def make_design_matrix(self):
        return self._bases(self.X)

")
