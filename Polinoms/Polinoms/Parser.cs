using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Polinoms
{
    class Parser
    {
        protected int pointer = 0;
        protected string SubString(string str)
        {
            return str.Substring(this.pointer);
        }

        protected int ParseNumber(string str, char startChar, char endChar)
        {
            int start = -1;
            int end = -1;
            for (int i = pointer; i < str.Length; i++)
            {
                if (str[i] == startChar)
                {
                    start = i + 1;
                }
                if (str[i] == endChar)
                {
                    end = i;
                    break;
                }
            }

            if ((start == -1) || (end == -1))
            {
                Console.WriteLine("The line has not been parsed correctly.");
                return -1;
            }

            this.pointer = end;

            string subString = str.Substring(start, end - start);
            return int.Parse(subString);
        }
        protected int ParsePower(string str)
        {
            if (this.pointer >= str.Length)
            {
                return 0;
            }

            int start = -1;
            int end = -1;
            for (int i = pointer; i < str.Length; i++)
            {
                if (str[i] == '^')
                {
                    start = i + 1;
                }
                if ((str[i] == '+') || (str[i] == '-'))
                {
                    end = i;
                    break;
                }
            }

            if (start == -1)
            {
                pointer++;
                return 1;
            }

            if (end == -1)
            {
                Console.WriteLine("The line has not been parsed correctly.");
            }

            this.pointer = end;

            string subString = str.Substring(start, end - start);

            if (subString.Length == 0)
            {
                return 1;
            }

            int number = int.Parse(subString);
            return number;
        }
        protected int ParseCoefficients(string str)
        {
            int start = -1;
            int end = -1;
            char sign = '0';
            int shift = 1;
            for (int i = pointer; i < str.Length; i++)
            {
                if ((str[i] == ' ') || (str[i] == '+') || (str[i] == '-'))
                {
                    sign = str[i];
                    start = i + 1;
                }
                if ((str[i] == '*') || (str[i] == 'x'))
                {
                    end = i;
                    if (str[i] == 'x')
                    {
                        shift = 0;
                    }
                    break;
                }
            }

            if (end == -1)
            {
                end = str.Length;
            }

            if (start == -1)
            {
                Console.WriteLine("The line has not been parsed correctly.");
                return -1;
            }

            this.pointer = end + shift;

            string subString = str.Substring(start, end - start);

            if (subString.Length == 0)
            {
                return (sign == '-') ? -1 : 1;
            }

            int number = int.Parse(subString);
            return (sign == '-') ? -number : number;
        }
        public cpolinom Inverve(cpolinom a)
        {
            cpolinom b = new cpolinom("", a.mod);
            for (int i = 0; i < a.Count; i++)
                b.Insert(0, a[i]);
            return b;
        }
        public cpolinom ParseString(string str)
        {
            this.pointer = 0;

            int baseField = ParseNumber(str, '(', '^');
            int powerField = ParseNumber(str, '^', ')');

            bool firstNumber = true;

            cpolinom polinom = new cpolinom("", baseField);
            List<int> coefficients = new List<int>();
            while ((this.pointer >= str.Length) == false)
            {
                int coefficient = ParseCoefficients(str);
                int power = ParsePower(str);

                if (firstNumber == true)
                {
                    polinom = new cpolinom(power, baseField);
                    firstNumber = false;
                }

                polinom[power] = coefficient;
            }

            return Inverve(polinom);
        }
    }
}
