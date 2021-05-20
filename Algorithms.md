# Algorithms

The list of algorithms currently implemented is as follows.

<table>
<thead>
  <tr>
    <th>Module</th>
    <th>Functions/Subroutines/Types</th>
    <th>Description</th>
    <th>See Also</th>
  </tr>
</thead>
<tbody>
  <tr>
    <td rowspan="3">Assert</td>
    <td colspan="3"></td>
  </tr>
  <tr>
    <td>assertEquals</td>
    <td>Asserts that two real numbers are equal.</td>
    <td></td>
  </tr>
  <tr>
    <td>assertEqualsWith</td>
    <td>Asserts that two real numbers up to n decimals.</td>
    <td></td>
  </tr>
  <tr>
    <td rowspan="5">Commons</td>
    <td colspan="3"></td>
  </tr>
  <tr>
    <td>erfun</td>
    <td>Approximation to Error function.</td>
    <td>https://www.wikiwand.com/en/Error_function</td>
  </tr>
  <tr>
    <td>erfunInv</td>
    <td>Approximation to Inverse Error function.</td>
    <td>https://www.wikiwand.com/en/Error_function</td>
  </tr>
  <tr>
    <td>ccdf</td>
    <td>Approximation to standard normal distribution.</td>
    <td>Abramowitz and Stegun, Formula 26.2.23</td>
  </tr>
  <tr>
    <td>roundNearN</td>
    <td>Round a real number to nearest up to n digits.</td>
    <td></td>
  </tr>
  <tr>
    <td>Constants</td>
    <td></td>
    <td>Frequently used math constants.</td>
    <td></td>
  </tr>
  <tr>
    <td>Distributions</td>
    <td>rbeta, rbinom, rcauchy, <br>rexp, rf, rgamma, rgeom,<br>rlognormal, rnorm, rpois,<br>rt, runif</td>
    <td>Random number generation for statistical distributions.</td>
    <td>1. Devroye's book: Chapter 9 &amp; 10<br><br>http://luc.devroye.org/chapter_nine.pdf, http://luc.devroye.org/chapter_ten.pdf<br><br>2. Kneusel's Random Numbers and Computers book.</td>
  </tr>
  <tr>
    <td rowspan="3">IO</td>
    <td colspan="3"></td>
  </tr>
  <tr>
    <td>printMatrix</td>
    <td>Subroutine for print two-dim. arrays to console.</td>
    <td></td>
  </tr>
  <tr>
    <td>writeCsv</td>
    <td>Subroutine for print two-dim. arrays to comma-seperated file.</td>
    <td></td>
  </tr>
  <tr>
    <td rowspan="10">Linear Algebra</td>
    <td colspan="3"></td>
  </tr>
  <tr>
    <td>cholesky</td>
    <td>Cholesky decomposition.</td>
    <td></td>
  </tr>
  <tr>
    <td>cofactor</td>
    <td>Cofactor matrix.</td>
    <td></td>
  </tr>
  <tr>
    <td>determinant</td>
    <td>Determinant of a matrix.</td>
    <td></td>
  </tr>
  <tr>
    <td>gauss</td>
    <td>Gaussian elimination for solving system of linear equations.</td>
    <td></td>
  </tr>
  <tr>
    <td>gramschmidt</td>
    <td>QR Decomposition with Gram-Schmidt method.</td>
    <td></td>
  </tr>
  <tr>
    <td>inverse</td>
    <td>Inverse of a matrix.</td>
    <td></td>
  </tr>
  <tr>
    <td>ludcmp</td>
    <td>LU decomposition.</td>
    <td></td>
  </tr>
  <tr>
    <td>minorMatrix</td>
    <td>Minor matrix.</td>
    <td></td>
  </tr>
  <tr>
    <td>transpose</td>
    <td>Transpose of a matrix.</td>
    <td></td>
  </tr>
  <tr>
    <td>Plots</td>
    <td>plot2d</td>
    <td>XY line plot with gnuplot.</td>
    <td>Install gnuplot.</td>
  </tr>
  <tr>
    <td>Random</td>
    <td>lcg</td>
    <td>Linear congruential generator for pseudo-random number generation.</td>
    <td></td>
  </tr>
  <tr>
    <td rowspan="3">RootFinding</td>
    <td colspan="3"></td>
  </tr>
  <tr>
    <td>secantMethod</td>
    <td>1D root finding.</td>
    <td></td>
  </tr>
  <tr>
    <td>broydenMethod</td>
    <td>N-dim. root finding [NOT IMPLEMENTED YET].</td>
    <td></td>
  </tr>
  <tr>
    <td>Testing</td>
    <td>Tests</td>
    <td>Minimal unit testing type.</td>
    <td></td>
  </tr>
</tbody>
</table>
