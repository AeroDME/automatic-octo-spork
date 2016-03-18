////////////////////////////////////////////////////////////////////////////////
// D. Everhart
// 2004 (original) Adapted form matrix.f (in same project)
// 2004 - PRESENT Minor updates.
////////////////////////////////////////////////////////////////////////////////
// The MIT License (MIT)
// 
// Copyright (c) 2016 Daniel Everhart
// 
// Permission is hereby granted, free of charge, to any person obtaining
// a copy of this software and associated documentation files (the 
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so, subject
// to the following conditions:
// 
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
// IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
// CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
// TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
// SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
// 
////////////////////////////////////////////////////////////////////////////////
using System;

/// <summary>
///   Used for matrix operations commonly found in linear algebra.
/// </summary>
/// <remarks>
///   The matrix is of double values.  While we could try to make this a
///   generic for use with double and float, some clown would try
///   to use it for int, and we could not restrict it.
/// </remarks>
public class Matrix 
{
  #region Fields

  protected const double TOL = 1.0e-12;
  protected double[] data;

  #endregion Fields

  #region Properties

  public int Rows { get; protected set; }
  public int Columns { get; protected set; }

  /// <summary>
  ///   Gets a value indicating whether the matrix dimensions are square.
  /// </summary>
  /// <value>
  ///   A boolean value representing the result of <c>(Rows == Columns)</c>.
  /// </value>
  public bool IsSquare
  {
    get { return Rows == Columns; }
  }

  /// <summary>
  ///   Gets/Sets a value in the Matrix object.
  /// </summary>
  /// <remarks>
  ///   Remember the matrix is 0 based.  That is to say that the first
  ///   row or column has index 0.
  /// </remarks>
  /// <value>
  ///   The value represents the member of the internal array.
  /// </value>
  /// <param name="row">Row of the desired value.</param>
  /// <param name="column">Column of the desired value.</param>
  /// <returns>
  ///   A double value which is at the given row, column.
  /// </returns>
  public virtual double this[int row, int column]
  {
    get
    {
      return data[row*Columns+column];
    }

    set
    {
      data[row * Columns + column] = value;
    }
  }

  #endregion Properties

  #region Constructors

  public Matrix(int size)  
    : this(size, 0.0) { }

  public Matrix(int size, double value)
    : this(size, size, value) { }

  public Matrix(int rows, int columns)
    : this(rows, columns, 0.0) { }

  public Matrix(int rows, int columns, double value)
  {
    Rows = rows;
    Columns = columns;
    data = new double[rows*columns];
    for (int i = 0; i < data.Length; i++) data[i] = value;
  }

  public Matrix(Matrix matrix)
    : this(matrix.Rows, matrix.Columns)
  {
    matrix.data.CopyTo(data, 0);
  }

  public Matrix(double[,] data)
    : this(data.GetLength(0), data.GetLength(1))
  {
    if (data != null)
    {
      for (int i = 0; i < Rows; i++)
        for (int j = 0; j < Columns; j++)
          this[i,j] = data[i, j];
    }
    else
    {
      throw new ArgumentNullException("data");
    }
  }

  #endregion Constructors

  #region Methods

  /// <summary>
  ///   Tests to see if matrix dimensions agree for matrix multiplication.
  /// </summary>
  /// <param name="matrix">The Matrix object to test.</param>
  /// <returns>
  ///   <b>true</b> if this <see cref="Matrix"/> can be multiplied by a
  ///   the given <see cref="Matrix"/>.
  /// </returns>
  /// <remarks>
  ///   Matricies may only be multiplied if the inner dimensions agree.
  ///   That is to say that the number of columns of the first matrix must
  ///   be the same as the number of rows of the second matrix.
  /// </remarks>
  public bool CanBeMultipliedBy(Matrix matrix)
  {
    return Columns == matrix.Rows;
  }

  /// <summary>
  ///   Adds the scalar double value to every element in the Matrix object.
  /// </summary>
  /// <param name="value">The value to add.</param>
  public void Add(double value)
  {
    for (int i = 0; i < this.data.Length; i++)
    {
      this.data[i] += value;
    }
  }

  /// <summary>
  ///   Subtracts the scalar double value to every element in the Matrix
  ///   object.
  /// </summary>
  /// <param name="value">The value to subtract.</param>
  public void Subtract(double value)
  {
    for (int i = 0; i < this.data.Length; i++)
    {
      this.data[i] -= value;
    }
  }

  /// <summary>
  ///   Multiplies the scalar double value times every element in the
  ///   Matrix object.
  /// </summary>
  /// <param name="value">The value by which to multiply.</param>
  public void Multiply(double value)
  {
    for (int i = 0; i < this.data.Length; i++)
    {
      this.data[i] *= value;
    }
  }

  /// <summary>
  ///   Divides the scalar double value times every element in the
  ///   Matrix object.
  /// </summary>
  /// <param name="value">The value by which to divide.</param>
  public void Divide(double value)
  {
    for (int i = 0; i < this.data.Length; i++)
    {
      this.data[i] /= value;
    }
  }

  /// <summary>
  ///   Adds the Matrix object matrix to this Matrix object.
  /// </summary>
  /// <param name="matrix">The matrix to be added.</param>
  public void Add(Matrix matrix)
  {
    if (matrix != null)
    {
      if (Rows == matrix.Rows && Columns == matrix.Columns)
      {
        for (int i = 0; i < data.Length; i++)
        {
          data[i] += matrix.data[i];
        }
      }
      else
      {
        throw new ArgumentException(
          "Matrix dimensions do not agree.", "matrix");
      }
    }
    else
    {
      throw new ArgumentNullException("matrix");
    }
  }

  /// <summary>
  ///   Subtracts the Matrix object matrix to this Matrix object.
  /// </summary>
  /// <param name="matrix">The matrix to be subtracted.</param>
  public void Subtract(Matrix matrix)
  {
    if (matrix != null)
    {
      if (Rows == matrix.Rows && Columns == matrix.Columns)
      {
        for (int i = 0; i < data.Length; i++)
        {
          data[i] -= matrix.data[i];
        }
      }
      else
      {
        throw new ArgumentException(
          "Matrix dimensions do not agree.", "matrix");
      }
    }
    else
    {
      throw new ArgumentNullException("matrix");
    }
  }

  /// <summary>
  ///   Transposes this matrix by swapping rows for columns.
  /// </summary>
  public Matrix Transpose()
  {
    Matrix matrix = new Matrix(Columns, Rows);
    for (int i = 0; i < Rows; i++)
      for (int j = 0; j < Columns; j++)
        matrix[j, i] = this[i, j];
    return matrix;
  }

  /// <summary>
  ///   Creates a new Matrix partition based on the indicies provided.
  /// </summary>
  /// <param name="indicies">Array of indicies of rows/columns to get.</param>
  /// <returns>
  ///   A new matrix with rows and columns specified by
  ///   <paramref name="indicies"/> in the same order as
  ///   <paramref name="indicies"/>.
  /// </returns>
  /// <remarks>
  ///   The list of indicies is used to decide which rows and columns
  ///   are pulled.  For example, if { 1, 3, 5} is passed, then rows
  ///   1,3,5 and columns 1,3,5 are returned in the new matrix.
  ///   <para/>
  ///   Additionally, rows and columns may be rearranged.  For example,
  ///   if {1, 5, 3} is passed, then then new matrix will be a 3X3 with
  ///   the second row pulled from row 5 of the original matrix, and the
  ///   throw pulled from row 3.
  /// </remarks>
  public Matrix Partition(int[] indicies)
  {
    if (indicies == null)
      throw new ArgumentNullException("indicies");
    Matrix t = new Matrix(indicies.Length, Rows);
    for (int i = 0; i < indicies.Length; i++) t[i, indicies[i]] = 1.0;
    return t * this * t.Transpose();
  }

  /// <summary>
  ///   Creates a new Matrix based on the partion provided.
  /// </summary>
  /// <param name="partition">The partition to use.</param>
  /// <returns>
  ///   A new Matrix partitioned from this based on
  ///   <paramref name="partition"/>.
  /// </returns>
  /// <remarks>
  ///   If <paramref name="partition"/> references an area of the Matrix
  ///   which is beyond the bounds, then the largest possible section is
  ///   returned.
  /// </remarks>
  public Matrix Partition(int row, int column, int rows, int columns)
  {
    Matrix matrix = new Matrix(rows, columns);
    for (int i = 0; i < rows; i++)
      for (int j = 0; j < columns; j++)
        matrix[i, j] = this[i + row, j + column];
    return matrix;
  }

  /// <summary>
  ///   Performs Gauss elimination on the matrix.
  /// </summary>
  public void GaussElimination()
  {
    for (int i = 0; i < Rows - 1; i++)
    {
      PartialPivot(i);
      GaussEliminateColumn(i);
    }
    BackSubstitution();
  }

  /// <summary>
  ///   Attempts to perform a partial pivoting on the matrix for the given
  ///   row.  First, a check is performed to determined if a pivot is
  ///   required for a Gaussian elimination to be performed on the following
  ///   rows.
  /// </summary>
  /// <param name="row">Row on which to perform pivot.</param>
  /// <remarks>
  ///   If the element at index (row,row) is zero (or within the tolerance),
  ///   then a partial pivot operation is attempted.  All rows following this
  ///   are searched until a row with a non-zero term at column index of the 
  ///   same value as <paramref name="row" /> is found.
  ///   <para/><para/>
  ///   <example>
  ///     Consider the following matrix A:<para/><para/>
  ///     <c>
  ///              | A11 A12 A13 A14 A15 A16 A17 |<para/>
  ///              | 0.0 A22 A23 A24 A25 A26 A27 |<para/>
  ///              | 0.0 A32 A33 A34 A35 A36 A37 |<para/>
  ///              | 0.0 A42 A43 A44 A45 A46 A47 |<para/>
  ///              | 0.0 A52 A53 A54 A55 A56 A57 |<para/>
  ///              | 0.0 A62 A63 A64 A65 A66 A67 |<para/>
  ///              | 0.0 A72 A73 A74 A75 A76 A77 |<para/>
  ///     </c>
  ///     <para/><para/>
  ///     If partial pivoting is invoked for the second row as in the
  ///     following code:
  ///     <para/><para/>
  ///     <code>
  ///     A.PartialPivot(1);
  ///     </code>
  ///     <para/><para/>
  ///     First, A22 is checked to see if it is zero.  If it is not zero,
  ///     nothing is done.  If it is zero, then the next rows are checked
  ///     until a nonzero number is found at the same column location
  ///     (A32, A42, A52, ..., A72).  Once a non-zero value is found,
  ///     the rows are swapped.
  ///   </example>
  ///   If no suitable row is found to swap, nothing is done.  There will
  ///   likely be a division by zero error, if an elimination is in progress.
  /// </remarks>
  public void PartialPivot(int row)
  {
    if (Math.Abs(this[row,row]) <= TOL)
    {
      int j;
      for (j = row + 1; j < Rows; j++)
        if (Math.Abs(this[j, row]) >= TOL) break;

      for (int k = row; k < Columns; k++)
      {
        double tmp = this[row,k];
        this[row,k] = this[j,k];
        this[j,k] = tmp;
      }
    }
  }

  /// <summary>
  ///   Loops through rows starting at row index equal to
  ///   <c><paramref name="column" /> + 1</c> continuing to the last row.
  ///   Each cycle zeros out the the element on the current column for the
  ///   particular row.
  /// </summary>
  /// <param name="column">The index of the column to be zeroed.</param>
  /// <remarks>
  ///   This method assumes that the columns with index less than
  ///   <paramref name="column" /> have already been zeroed.  So the first
  ///   row must be at row <c><paramref name="column" /> + 1</c>
  ///   <para/><para/>
  ///   <example>
  ///     Consider the following matrix A:<para/><para/>
  ///     <c>
  ///              | A11 A12 A13 A14 A15 |<para/>
  ///              | 0.0 A22 A23 A24 A25 |<para/>
  ///              | 0.0 A32 A33 A34 A35 |<para/>
  ///              | 0.0 A42 A43 A44 A45 |<para/>
  ///              | 0.0 A52 A53 A54 A55 |<para/>
  ///     </c>
  ///     <para/><para/>
  ///     The second column (index 1) is zeroed with the following call
  ///     <para/><para/>
  ///     <code>
  ///     A.GaussEliminateColumn(1);
  ///     </code>
  ///     <para/><para/>
  ///     Each row after row 1 (second row) is reduced by a factor (A32/A22)
  ///     times row 1 which will be a result in the item at column 1 being
  ///     zero.  The above call will result in the following change to the
  ///     matrix:<para/><para/>
  ///     <c>
  ///          <t/>| A11 A12 A13 A14 A15 |<para/>
  ///              | 0.0 A22 A23 A24 A25 |<para/>
  ///              | 0.0 0.0 A33 A34 A35 |<para/>
  ///              | 0.0 0.0 A43 A44 A45 |<para/>
  ///              | 0.0 0.0 A53 A54 A55 |<para/>
  ///     </c>
  ///   </example>
  ///   Repeated to calls to this method may be used to reduce a matrix
  ///   to an upper triangular matrix.
  /// </remarks>
  public void GaussEliminateColumn(int column)
  {
    for (int j = column + 1; j < Rows; j++)
    {
      this.GaussEliminateRow(j, column);
    }
  }

  /// <summary>
  ///   For a given row subtracts a scaled version of the row at index equal
  ///   to <paramref name="column" /> to result in that row having a zero 
  ///   at column.
  /// </summary>
  /// <param name="row">The row to be modified.</param>
  /// <param name="column">The column to be zeroed.</param>
  /// <remarks>
  ///   This method assumes that the columns with index less than
  ///   <paramref name="column" /> have already been zeroed.  So the first
  ///   row must be at row <c><paramref name="column" /> + 1</c>
  ///   <para/><para/>
  ///   <example>
  ///     Consider the following matrix A:<para/><para/>
  ///     <c>
  ///              | A11 A12 A13 A14 A15 |<para/>
  ///              | 0.0 A22 A23 A24 A25 |<para/>
  ///              | 0.0 A32 A33 A34 A35 |<para/>
  ///              | 0.0 A42 A43 A44 A45 |<para/>
  ///              | 0.0 A52 A53 A54 A55 |<para/>
  ///     </c>
  ///     <para/><para/>
  ///     The third row (index 2) is to be modified with the second 
  ///     column (index 1) being zeroed with the following call
  ///     <para/><para/>
  ///     <code>
  ///     A.GaussEliminateRow(2,1);
  ///     </code>
  ///     <para/><para/>
  ///     Row 2 after row 1 (second row) is reduced by a factor (A32/A22)
  ///     times row 1 which will be a result in the item at column (2,1)
  ///     being zero.  The above call will result in the following change
  ///     to the matrix:<para/><para/>
  ///     <c>
  ///              | A11 A12 A13 A14 A15 |<para/>
  ///              | 0.0 A22 A23 A24 A25 |<para/>
  ///              | 0.0 0.0 A33 A34 A35 |<para/>
  ///              | 0.0 A42 A43 A44 A45 |<para/>
  ///              | 0.0 A52 A53 A54 A55 |<para/>
  ///     </c>
  ///   </example>
  /// </remarks>
  public void GaussEliminateRow(int row, int column)
  {
    // We only need to perform elimination on this row if the item is not
    // already zero.  If it is zero, we can just skip this line.
    if (Math.Abs(this[row, column]) >= TOL)
    {
      double ratio = this[row, column] / this[column, column];
      for (int k = 0; k < Columns; k++)
      {
        if (k < column)
        {
          this[row, k] = 0.0;
        }
        else
        {
          this[row, k] -= ratio * this[column, k];
        }
      }
    }
    else
    {
      // Because we have assumed this to be zero, we will actually set it.
      this[row, column] = 0.0;
    }
  }

  /// <summary>
  ///   Assumes an upper triangular matrix
  ///   augmented by one or more vectors.  Solves for each vector.
  /// </summary>
  /// <remarks>
  ///   TODO Add a better description.
  /// </remarks>
  public void BackSubstitution()
  {
    // Back Substitution
    for (int i = Rows; i < Columns; i++)
    {
      for (int j = Rows - 1; j >= 0; j--)
      {
        double sum = 0.0;
        for (int k = Rows - 1; k > j; k--)
        {
          sum += this[j,k] * this[k,i];
        }

        this[j,i] = (this[j,i] - sum) / this[j,j];
      }
    }
  }

  /// <summary>
  ///   Inverts this matrix.
  /// </summary>
  public Matrix Inverse()
  {
    if (!this.IsSquare)
      throw new ArgumentException("Nonsquare matrix cannot be inverted.");

    // Create a new Matrix which is this matrix augmented with an
    // identity matrix.
    Matrix mat = new Matrix(Rows, Columns*2);

    for (int i = 0; i < Rows; i++)
    {
      for (int j = 0; j < Columns; j++)
      {
        // Original part of matrix.
        mat[i,j] = this[i,j];

        // Identity matrix.
        if (i == j) mat[i, j + Columns] = 1.0;
        else        mat[i, j + Columns] = 0.0;
      }
    }

    // Run Gauss elimination
    mat.GaussElimination();

    // Return partition which is the inverse.
    return mat.Partition(0, Columns, Rows, Columns);
  }

  #endregion Methods

  #region Static Methods

  /// <summary>
  ///   Produces a Matrix with the given size populated with random
  ///   values.
  /// </summary>
  /// <returns>
  ///   A new <see cref="Matrix"/> object populated with random values.
  /// </returns>
  /// <param name="rows">The rows in the new matrix.</param>
  /// <param name="columns">The columns in the new matrix.</param>
  /// <remarks>
  ///   Values are produced with a call to
  ///   <see cref="System.Random.NextDouble()"/>.
  /// </remarks>
  public static Matrix Random(int rows, int columns)
  {
    Matrix m = new Matrix(rows, columns);
    Random r = new Random();
    for (int i = 0; i < m.data.Length; i++) m.data[i] = r.NextDouble();
    return m;
  }

  /// <summary>
  ///   Produces a square Matrix with the given size populated with random
  ///   values.
  /// </summary>
  /// <returns>
  ///   A new <see cref="Matrix"/> object populated with random values.
  /// </returns>
  /// <param name="size">The size of the matrix to create.</param>
  /// <remarks>
  ///   Values are produced with a call to
  ///   <see cref="System.Random.NextDouble()"/>.
  /// </remarks>
  public static Matrix Random(int size)
  {
    return Random(size, size);
  }

  /// <summary>
  ///   Produces an Identity Matrix with the given size.
  /// </summary>
  /// <returns>
  ///   A new <see cref="Matrix"/> object which is an identity matrix.
  /// </returns>
  /// <param name="size">The size of the matrix to create.</param>
  /// <remarks>
  ///   By definition, and identity matrix is square which is why only
  ///   an int is required.
  /// </remarks>
  public static Matrix Identity(int size)
  {
    Matrix m = new Matrix(size);
    for (int i = 0; i < size; i++)  m[i,i] = 1.0;
    return m;
  }

  /// <summary>
  ///   Returns a new Matrix object where every element is equal to the
  ///   corresponding element of matrix plus value.
  /// </summary>
  /// <param name="matrix">The matrix to which value is added.</param>
  /// <param name="value">The value to be added.</param>
  /// <returns>
  ///   A <see cref="Matrix"/> object equal to <c>matrix + value</c>.
  /// </returns>
  public static Matrix Add(Matrix matrix, double value)
  {
    Matrix m = new Matrix(matrix);
    m.Add(value);
    return m;
  }

  /// <summary>
  ///   Returns a new Matrix object where every element is equal to the
  ///   corresponding element of matrix minus value.
  /// </summary>
  /// <param name="matrix">The matrix from which value is subtracted.</param>
  /// <param name="value">The value to be subtracted.</param>
  /// <returns>
  ///   A <see cref="Matrix"/> object equal to <c>matrix - value</c>.
  /// </returns>
  public static Matrix Subtract(Matrix matrix, double value)
  {
    Matrix m = new Matrix(matrix);
    m.Subtract(value);
    return m;
  }

  /// <summary>
  ///   Returns a new Matrix object where every element is equal to the
  ///   corresponding element of matrix multiplied by value.
  /// </summary>
  /// <param name="matrix">The matrix by which value is multiplied.</param>
  /// <param name="value">The value multiplied.</param>
  /// <returns>
  ///   A <see cref="Matrix"/> object equal to <c>matrix * value</c>.
  /// </returns>
  public static Matrix Multiply(Matrix matrix, double value)
  {
    Matrix m = new Matrix(matrix);
    m.Multiply(value);
    return m;
  }

  /// <summary>
  ///   Returns a new Matrix object where every element is equal to the
  ///   corresponding element of matrix divided by value.
  /// </summary>
  /// <param name="matrix">The matrix by which value is divided.</param>
  /// <param name="value">The value divided.</param>
  /// <returns>
  ///   A <see cref="Matrix"/> object equal to <c>matrix / value</c>.
  /// </returns>
  public static Matrix Divide(Matrix matrix, double value)
  {
    Matrix m = new Matrix(matrix);
    m.Divide(value);
    return m;
  }

  /// <summary>
  ///   Returns a new Matrix object which is the sum of matrix1 and matrix2
  ///   [ matrix1 ] + [ matrix2 ].
  /// </summary>
  /// <param name="matrix1">The first matrix to be aded.</param>
  /// <param name="matrix2">The second matrix to be aded.</param>
  /// <returns>
  ///   A <see cref="Matrix"/> object equal to <c>matrix1 + matrix2</c>.
  /// </returns>
  public static Matrix Add(Matrix matrix1, Matrix matrix2)
  {
    Matrix m = new Matrix(matrix1);
    m.Add(matrix2);
    return m;
  }

  /// <summary>
  ///   Returns a new Matrix object which is the difference of matrix1
  ///   and matrix2
  ///   [ matrix1 ] - [ matrix2 ].
  /// </summary>
  /// <param name="matrix1">The first matrix.</param>
  /// <param name="matrix2">The second matrix.</param>
  /// <returns>
  ///   A <see cref="Matrix"/> object equal to <c>matrix1 - matrix2</c>.
  /// </returns>
  public static Matrix Subtract(Matrix matrix1, Matrix matrix2)
  {
    Matrix m = new Matrix(matrix1);
    m.Subtract(matrix2);
    return m;
  }

  /// <summary>
  ///   Returns a new Matrix object which is the product of matrix1
  ///   and matrix2
  ///   [ matrix1 ] * [ matrix2 ].
  /// </summary>
  /// <param name="matrix1">The first matrix.</param>
  /// <param name="matrix2">The second matrix.</param>
  /// <returns>
  ///   A new <see cref="Matrix"/> which is the product of
  ///   matrix1 X matrix2.
  /// </returns>
  /// <remarks>
  ///   For the matricies to be mulitplied, the number of columns in matrix1
  ///   must match the number of rows in matrix2.  This is checked using
  ///   the following method:
  ///   <see cref="Matrix.CanBeMultipliedBy(Matrix)"/>
  ///   The user may use it as well, for checking dimensions..
  /// </remarks>
  public static Matrix Multiply(Matrix matrix1, Matrix matrix2)
  {
    if (!matrix1.CanBeMultipliedBy(matrix2))
    {
      throw new ArgumentException(
          "Matrix dimensions do not agree.", "matrix");
    }
    Matrix product = new Matrix(matrix1.Rows, matrix2.Columns);
    for (int i = 0; i < product.Rows; i++)
    {
      for (int j = 0; j < product.Columns; j++)
      {
        double sum = 0.0;
        for (int k = 0; k < matrix1.Columns; k++)
        {
          sum += matrix1[i,k] * matrix2[k,j];
        }
        product[i,j] = sum;
      }
    }
    return product;
  }

  #endregion Static Methods

  #region Operator Overrides

  /// <summary>
  ///   Returns a new Matrix object where every element is equal to the
  ///   corresponding element of matrix plus value.
  ///   Calls <see cref="Matrix.Add(Matrix,double)"/>.
  /// </summary>
  /// <param name="matrix">The matrix to which value is added.</param>
  /// <param name="value">The value to be added.</param>
  /// <returns>
  ///   A <see cref="Matrix"/> object equal to <c>matrix + value</c>.
  /// </returns>
  public static Matrix operator +(Matrix matrix, double value)
  {
    return Matrix.Add(matrix, value);
  }

  /// <summary>
  ///   Returns a new Matrix object where every element is equal to the
  ///   corresponding element of matrix minus value.
  ///   Calls <see cref="Matrix.Subtract(Matrix,double)"/>.
  /// </summary>
  /// <param name="matrix">The matrix from which value is subtracted.</param>
  /// <param name="value">The value to be subtracted.</param>
  /// <returns>
  ///   A <see cref="Matrix"/> object equal to <c>matrix - value</c>.
  /// </returns>
  public static Matrix operator -(Matrix matrix, double value)
  {
    return Matrix.Subtract(matrix, value);
  }

  /// <summary>
  ///   Returns a new Matrix object where every element is equal to the
  ///   corresponding element of matrix multiplied by value.
  ///   Calls <see cref="Matrix.Multiply(Matrix,double)"/>.
  /// </summary>
  /// <param name="matrix">The matrix by which value is multiplied.</param>
  /// <param name="value">The value multiplied.</param>
  /// <returns>
  ///   A <see cref="Matrix"/> object equal to <c>matrix * value</c>.
  /// </returns>
  public static Matrix operator *(Matrix matrix, double value)
  {
    return Matrix.Multiply(matrix, value);
  }

  /// <summary>
  ///   Returns a new Matrix object where every element is equal to the
  ///   corresponding element of matrix divided by value.
  ///   Calls <see cref="Matrix.Divide(Matrix,double)"/>.
  /// </summary>
  /// <param name="matrix">The matrix by which value is divided.</param>
  /// <param name="value">The value divided.</param>
  /// <returns>
  ///   A <see cref="Matrix"/> object equal to <c>matrix / value</c>.
  /// </returns>
  public static Matrix operator /(Matrix matrix, double value)
  {
    return Matrix.Divide(matrix, value);
  }

  /// <summary>
  ///   Returns a new Matrix object which is the sum of matrix1 and matrix2
  ///   [ matrix1 ] + [ matrix2 ]
  ///   <see cref="Matrix.Add(Matrix,Matrix)"/>.
  /// </summary>
  /// <param name="matrix1">The first matrix to be aded.</param>
  /// <param name="matrix2">The second matrix to be aded.</param>
  /// <returns>
  ///   A <see cref="Matrix"/> object equal to <c>matrix1 + matrix2</c>.
  /// </returns>
  public static Matrix operator +(Matrix matrix1, Matrix matrix2)
  {
    return Matrix.Add(matrix1, matrix2);
  }

  /// <summary>
  ///   Returns a new Matrix object which is the difference of matrix1
  ///   and matrix2
  ///   [ matrix1 ] - [ matrix2 ].
  /// </summary>
  /// <param name="matrix1">The first matrix.</param>
  /// <param name="matrix2">The second matrix.</param>
  /// <returns>
  ///   A <see cref="Matrix"/> object equal to <c>matrix1 - matrix2</c>.
  /// </returns>
  public static Matrix operator -(Matrix matrix1, Matrix matrix2)
  {
    return Matrix.Subtract(matrix1, matrix2);
  }

  /// <summary>
  ///   Returns a new Matrix object which is the matrix product of matrix1
  ///   and matrix2:  [ matrix1 ] X [ matrix2 ].
  /// </summary>
  /// <param name="matrix1">The first matrix.</param>
  /// <param name="matrix2">The second matrix.</param>
  /// <remarks>
  ///   For the matricies to be mulitplied, the number of columns in matrix1
  ///   must match the number of rows in matrix2.  This is checked using
  ///   the following method:
  ///   <see cref="Matrix.CanBeMultipliedBy(Matrix)"/>
  ///   The user may use it as well, for checking dimensions..
  /// </remarks>
  /// <returns>
  ///   A <see cref="Matrix"/> object equal to <c>matrix1 X matrix2</c>.
  /// </returns>
  public static Matrix operator *(Matrix matrix1, Matrix matrix2)
  {
    return Matrix.Multiply(matrix1, matrix2);
  }

  /// <summary>
  ///   Returns true if both Matrix objects are equal.
  /// </summary>
  /// <param name="matrix1">The first Matrix object.</param>
  /// <param name="matrix2">The second Matrix object.</param>
  /// <returns>
  ///   <c>true</c> if <paramref name="matrix1"/> is equal to
  ///   <paramref name="matrix2"/>; otherwise <c>false</c>.
  /// </returns>
  /// <remarks>
  ///   This method calls <see cref="Object.ReferenceEquals(object,object)"/>
  ///   to see if <paramref name="matrix1"/> and <paramref name="matrix2"/>
  ///   are referencing null.  If both are null, true is returned.
  ///   if only one is null, false is returned. Otherwise,
  ///   <see cref="Equals(Matrix)"/> is called and the result is returned.
  /// </remarks>
  public static bool operator ==(Matrix matrix1, Matrix matrix2)
  {
    if (ReferenceEquals(matrix1, null))
    {
      if (ReferenceEquals(matrix2, null))
      {
        return true;
      }
      else
      {
        return false;
      }
    }

    return matrix1.Equals(matrix2);
  }

  /// <summary>
  ///   Returns true if both Matrix objects are not equal.
  /// </summary>
  /// <param name="matrix1">The first Matrix object.</param>
  /// <param name="matrix2">The second Matrix object.</param>
  /// <returns>
  ///   <c>true</c> if <paramref name="matrix1"/> is not equal to
  ///   <paramref name="matrix2"/>; otherwise, <c>false</c>.
  /// </returns>
  /// <remarks>
  ///   This method calls <see cref="Object.ReferenceEquals(object,object)"/>
  ///   to see if <paramref name="matrix1"/> and <paramref name="matrix2"/>
  ///   are referencing null.  If both are null, false is returned.
  ///   if only one is null, true is returned. Otherwise,
  ///   <see cref="Equals(Matrix)"/> is called and the result is negated
  ///   and returned.
  /// </remarks>
  public static bool operator !=(Matrix matrix1, Matrix matrix2)
  {
    if (ReferenceEquals(matrix1, null))
    {
      if (ReferenceEquals(matrix2, null))
      {
        return false;
      }
      else
      {
        return true;
      }
    }

    return !matrix1.Equals(matrix2);
  }

  #endregion Operator Overrides

  #region Object Overrides

  #region Methods

  /// <summary>
  ///   Overrides <see cref="Object.Equals(Object)" />.
  /// </summary>
  /// <param name="obj">Other object with which to check equality.</param>
  /// <returns>
  ///   <c>true</c> if <paramref name="obj"/> is a <see cref="Matrix"/> and
  ///   is equal to the other; otherwise, <c>false</c>.
  /// </returns>
  /// <remarks>
  ///   If <paramref name="obj"/> is of type <see cre="Matrix"/>, then
  ///   this method returns the result of a call to
  ///   <see cref="Equals(Matrix)"/>.  Otherwise, it returns false.
  /// </remarks>
  public override bool Equals(object obj)
  {
    Matrix m = (Matrix)obj;
    if (m == null)
    {
      return false;
    }

    return Equals(m);
  }

  /// <summary>
  ///   Overrides <see cref="Object.Equals(Object)" />.
  /// </summary>
  /// <returns>
  ///   An integer value that specifies the hash code for this object.
  /// </returns>
  public override int GetHashCode()
  {
    return data.GetHashCode() ^ 
           Rows.GetHashCode() ^ 
           Columns.GetHashCode();
  }

  /// <summary>
  ///   Converts the object to a human-readable string.
  /// </summary>
  /// <returns>
  ///   Returns a string conatining all of the values in the matrix.
  /// </returns>
  public override string ToString()
  {
    string str = string.Empty;
    NumericFormatter nf = new NumericFormatter();

    for (int i = 0; i < Rows; i++)
    {
      for (int j = 0; j < Columns; j++)
      {
        str += string.Format(nf, "{0:8},", this[i, j]);
      }
      
      str += Environment.NewLine;
    }
    
    return str;
  }

  #endregion Methods

  #endregion Object Overrides

  #region IEquatable<Matrix> Implementation

  #region Methods

  /// <summary>
  ///   Implements <see cref="IEquatable&lt;T&gt;.Equals(T)" />
  ///   Returns true only if all members are exactly the same.  We may want
  ///   to decide if this should return true if the values are the same
  ///   within some tolerance.
  /// </summary>
  /// <param name="other">Other matrix.</param>
  /// <returns>
  ///   <c>true</c> if this matrix is equal to the other; otherwise,
  ///   <c>false</c>.
  /// </returns>
  public bool Equals(Matrix other)
  {
    if (ReferenceEquals(other, null))
    {
      return false;
    }

    if (ReferenceEquals(other, this))
    {
      return true;
    }

    if (this.data.Length != other.data.Length)
    {
      return false;
    }

    if (Rows != other.Rows || Columns != other.Columns)
    {
      return false;
    }

    for (int i = 0; i < this.data.Length; i++)
    {
      if (this.data[i] != other.data[i])
      {
        return false;
      }
    }

    return true;
  }

  #endregion Methods

  #endregion IEquatable<Matrix> Implementation

}
