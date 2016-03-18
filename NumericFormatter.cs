////////////////////////////////////////////////////////////////////////////////
// NumericFormatter
// D. Everhart
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
using System.Globalization;

/// <summary>
///   Used for formatting fields in a manner based on field width.
/// </summary>
/// <remarks>
///   For real numbers (float, double), the maximum possible precision is
///   given for a particular field width.  If no field width is supplied,
///   then 8 is assumed.
/// </remarks>
public class NumericFormatter : ICustomFormatter, IFormatProvider
{
  #region Fields

  /// <summary>
  ///   Values with magnitudes less than this are considered zero.
  /// </summary>
  private double tolerance = 1.0e-10;

  #endregion Fields

  #region Constructors
  
  /// <summary>
  ///   Initializes a new instance of the FieldFormatter class with a 
  ///   tolerance of <c>1.0e-10</c>.
  /// </summary>
  public NumericFormatter()
  {
  }
  
  /// <summary>
  ///   Initializes a new instance of the FieldFormatter class with the
  ///   <paramref name="tolerance"/> specified.
  /// </summary>
  /// <param name="tolerance">The tolerance to be used.</param>
  public NumericFormatter(double tolerance)
  {
    this.tolerance = Math.Abs(tolerance);
  }
  
  #endregion Constructors

  #region Properties

  /// <summary>
  ///   Gets or sets the tolerance for determining values which are
  ///   considered zero.
  /// </summary>
  /// <value>
  ///   A double value representing the smallest value which is considered
  ///   non-zero.  The default is 1.0e-10.
  /// </value>
  public double Tolerance
  {
    get { return this.tolerance; }
    set { this.tolerance = value; }
  }

  #endregion Properties

  #region Static Methods

  /// <summary>
  ///   Creates a string representation of <paramref name="val"/> of length
  ///   <paramref name="width"/> or less.
  /// </summary>
  /// <param name="val">The value to be converted to a string.</param>
  /// <param name="width">The string width.</param>
  /// <param name="tolerance" >
  ///   The tolerance of magnitudes considered zero.
  /// </param>
  /// <returns>
  ///   A string representation of <paramref name="val"/> of length
  ///   <paramref name="width"/> or less.
  /// </returns>
  /// <remarks>
  ///   If it is not possible to create a string representation of
  ///   <paramref name="val"/> with a length of <paramref name="width"/> or 
  ///   less, then a string of '#' characters of length
  ///   <paramref name="width"/> is returned.
  /// </remarks>
  private static string BuildString(float val, int width, double tolerance)
  {
    return BuildString((double)val, width, tolerance);
  }

  /// <summary>
  ///   Creates a string representation of <paramref name="val"/> of length
  ///   <paramref name="width"/> or less.
  /// </summary>
  /// <param name="val">The value to be converted to a string.</param>
  /// <param name="width">The string width.</param>
  /// <param name="tolerance" >
  ///   The tolerance of magnitudes considered zero.
  /// </param>
  /// <returns>
  ///   A string representation of <paramref name="val"/> of length
  ///   <paramref name="width"/> or less.
  /// </returns>
  /// <remarks>
  ///   If it is not possible to create a string representation of
  ///   <paramref name="val"/> with a length of <paramref name="width"/> or
  ///   less, then a string of '#' characters of length 
  ///   <paramref name="width"/> is returned.
  /// </remarks>
  private static string BuildString(double val, int width, double tolerance)
  {
    CultureInfo ci = new CultureInfo("en-US");
    int chars = width;
    
    // Check Tolerance
    if (System.Math.Abs(val) < ((double)tolerance)) val = 0.0;
    
    // Initial formatting is to attempt to use entire length.
    string strVal = val.ToString("g" + chars.ToString(ci), ci);
    if (!strVal.Contains(".")) strVal = InsertDecimal(strVal);
    
    // If the string is too long, decrement 'chars' to
    // shorten return string.
    while ((strVal.Length > width) && (chars > 0))
    {
      strVal = val.ToString("g" + (--chars).ToString(ci), ci);
      if (!strVal.Contains(".")) strVal = InsertDecimal(strVal);
    }
    
    // If 'chars' was decremented to zero or less, then we need
    // to build the string which is the error string.
    if (chars <= 0) strVal = new string('#', width);
    
    return strVal;
  }

  /// <summary>
  ///   Creates a string representation of <paramref name="val"/> of length
  ///   <paramref name="width"/> or less.
  /// </summary>
  /// <param name="val">The value to be converted to a string.</param>
  /// <param name="width">The string width.</param>
  /// <returns>
  ///   A string representation of <paramref name="val"/> of length
  ///   <paramref name="width"/> or less.
  /// </returns>
  /// <remarks>
  ///   If it is not possible to create a string representation of
  ///   <paramref name="val"/> with a length of <paramref name="width"/> or
  ///   less, then a string of '#' characters of length
  ///   <paramref name="width"/> is returned.
  /// </remarks>
  private static string BuildString(int val, int width)
  {
    return BuildString((long)val, width);
  }

  /// <summary>
  ///   Creates a string representation of <paramref name="val"/> of length
  ///   <paramref name="width"/> or less.
  /// </summary>
  /// <param name="val">The value to be converted to a string.</param>
  /// <param name="width">The string width.</param>
  /// <returns>
  ///   A string representation of <paramref name="val"/> of length
  ///   <paramref name="width"/> or less.
  /// </returns>
  /// <remarks>
  ///   If it is not possible to create a string representation of
  ///   <paramref name="val"/> with a length of <paramref name="width"/> or
  ///   less, then a string of '#' characters of length
  ///   <paramref name="width"/> is returned.
  /// </remarks>
  private static string BuildString(long val, int width)
  {
    CultureInfo ci = new CultureInfo("en-US");
    
    // Attempt to format string.
    string strVal = val.ToString("#", ci);
    
    // If the string is too long, return error string.
    if (strVal.Length > width)
    {
      strVal = string.Empty;
      while (strVal.Length < width)
      {
        strVal += "#";
      }
    }
    
    return strVal;
  }

  /// <summary>
  ///   Inserts a decimal into <paramref name="str"/> assuming str
  ///   represents a real number (double or float).
  /// </summary>
  /// <param name="str">The string in which to insert the decimal.</param>
  /// <returns>
  ///   A new string with the decimal inserted.
  /// </returns>
  /// <remarks>
  ///   Basically, this methods loops through the string, and finds the first
  ///   end of digit characters inserting a decimal in the proper place.
  ///   This is either at the end of the string of numbers, or immediately
  ///   before a non-digit character is encountered (e,E,+,-).
  ///   TODO: We may want to insert a ".0" so that we don't have numbers
  ///         which end in ".".  This is because we may have problems with
  ///         IronPython parsing numbers.
  /// </remarks>
  private static string InsertDecimal(string str)
  {
    if (!str.Contains("."))
    {
      if (str.IndexOf("e") > -1)
      {
        return str.Replace("e", ".e");
      }
      if (str.IndexOf("E") > -1)
      {
        return str.Replace("E", ".E");
      }
      return str + ".";
    }
    return str;      
  }

  #endregion Static Methods

  #region IFormatProvider implementation

  /// <summary>
  ///   Returns an object that provides formatting services for the specified
  ///   type.
  /// </summary>
  /// <param name="formatType">
  ///   The format type for which to get the object
  ///   providing formatting services.
  /// </param>
  /// <returns>
  ///   If formatType == typeof ( ICustomFormatter ), the return value is this
  ///   object.
  /// </returns>
  /// <remarks>
  ///   Only returns an object if formatType is typeof ( ICustomFormatter ).
  /// </remarks>
  public object GetFormat(Type formatType)
  {
    if (formatType == typeof(ICustomFormatter))
    {
      return this;
    }
    else
    {
      return null;
    }
  }

  #endregion IFormatProvider implementation

  #region ICustomFormatter implementation

  /// <summary>
  ///   Converts the value of a specified object to an equivalent string
  ///   representation using specified format and culture specific
  ///   formatting information.
  /// </summary>
  /// <param name="format">
  ///   A format string containing formatting specifications.
  /// </param>
  /// <param name="arg">An object to format.</param>
  /// <param name="formatProvider">
  ///   An object that supplies format information about the current
  ///   instance.  Currently, this parameter is ignored.
  /// </param>
  /// <returns>
  ///   A string representation of <paramref name="arg"/> formatted as
  ///   specified by <paramref name="format"/>.
  /// </returns>
  /// <remarks>
  ///   This implementation does not use <paramref name="formatProvider"/>.
  ///   See
  ///   <see cref="ICustomFormatter.Format(string,object,IFormatProvider)"/>
  ///   for more details.
  /// </remarks>
  public string Format( string format, 
                        object arg, 
                        IFormatProvider formatProvider)
  {
    // Get field length
    int len = 8;
    if (format != null)
    {
      if (!int.TryParse(format, out len))
      {
        len = 8;
      }
    }

    bool leftJustify = len < 0;
    len = System.Math.Abs(len);

    // Default string
    string str = string.Empty;
    //while (str.Length < len) str += "#";

    // These are the data types we handle.
    if (arg is float)
    {
      str = BuildString((float)arg, len, this.tolerance);
    }
    else if (arg is double)
    {
      str = BuildString((double)arg, len, this.tolerance);
    }
    else if (arg is int)
    {
      str = BuildString((int)arg, len);
    }
    else if (arg is long)
    {
      str = BuildString((long)arg, len);
    }
    else if (arg is string)
    {
      str = arg.ToString();
      while (str.Length < len) str += " ";
      str = str.Substring(0, len);
    }
    
    str = str.Trim();
    if (leftJustify)
    {
      while (str.Length < len) str += " ";
    }
    else
    {
      while (str.Length < len) str = " " + str;
    }
    
    return str;
  }

  #endregion ICustomFormatter implementation
}
