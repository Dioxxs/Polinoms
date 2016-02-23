using System;
using System.Numerics;

namespace Polinoms
{
    public class FFT
    {
        #region Приватные методы
        /// <summary>
        /// Вычисление поворачивающего модуля e^(-i*2*PI*k/N)
        /// </summary>
        /// <param name="k">номер</param>
        /// <param name="N">количество</param>
        /// <param name="direct">прямое или обратное преобразование</param>
        /// <returns></returns>
        private static Complex w(int k, int N, bool direct)
        {
            if (k % N == 0) return 1;
            double arg = (direct ? -2 : 2) * Math.PI * k / N;
            Complex w = new Complex(Math.Cos(arg), Math.Sin(arg));
            return w;
        }
        /// <summary>
        /// Возвращает спектр сигнала
        /// </summary>
        /// <param name="x">Массив значений сигнала. Количество значений должно быть степенью 2</param>
        /// <param name="direct">прямое или обратное преобразование</param>
        /// <returns>Массив со значениями спектра сигнала</returns>
        private static Complex[] _fft(Complex[] x, bool direct)
        {
            Complex[] X;
            int N = x.Length;
            if (N == 1)
            {
                X = new Complex[1];
                X[0] = x[0];
            }
            else
            {
                Complex[] x_even = new Complex[N / 2];
                Complex[] x_odd = new Complex[N / 2];
                for (int i = 0; i < N / 2; i++)
                {
                    x_even[i] = x[2 * i];
                    x_odd[i] = x[2 * i + 1];
                }
                Complex[] X_even = _fft(x_even, direct);
                Complex[] X_odd = _fft(x_odd, direct);
                X = new Complex[N];
                for (int i = 0; i < N / 2; i++)
                {
                    X[i] = X_even[i] + w(i, N, direct) * X_odd[i];
                    X[i + N / 2] = X_even[i] - w(i, N, direct) * X_odd[i];
                }

            }
            return X;
        }
        /// <summary>
        /// Возвращает спектр сигнала, в реализации теперь не используется рекурсия, минимизированое количество операций
        /// </summary>
        /// <param name="x">Массив значений сигнала. Количество значений должно быть степенью 2</param>
        /// <param name="direct">прямое или обратное преобразование</param>
        /// <returns>Массив со значениями спектра сигнала</returns>
        private static Complex[] _fft2(Complex[] x, bool direct)
        {
            int N = x.Length;
            Complex[] X = new Complex[N];
            int k = 0; //длина в битах  = lg(2)(N)
            while ((1 << k) < N) k++;

            //вспомагательный массив перестановок
            int[] rev = new int[N];
            rev[0] = 0;

            int high1 = -1;
            for (int i = 1; i < N; i++)
            {
                if ((i & (i - 1)) == 0) // Проверка на степень двойки. Если i ей является, то i-1 будет состоять из кучи единиц.
                    high1++;
                rev[i] = rev[i ^ (1 << high1)]; // Переворачиваем остаток
                rev[i] |= (1 << (k - high1 - 1)); // Добавляем старший бит
            }

            for (int i = 0; i < N; i++)
                X[i] = x[rev[i]];

            //сейчас в X находятся те же коеффициенты х, но в другом порядке 

            //l - количество элементов в блоке, при каждой итерации размер увеличивается в 2 раза
            for (int l = 1; l < N; l <<= 1)
            {
                Complex[] tmp = new Complex[N];//временный массив преобразований
                int l2 = l << 1; //= l * 2

                for (int i = 0; i < N; i += l2)
                {
                    int p1 = i,//начало первого блока
                        p2 = i + l,//начало второго блока
                        end1 = p2;//конец первого блока

                    for (int j = 0; j < l2; j++)
                    {
                        tmp[i + j] = X[p1] + w(j, l2, direct) * X[p2];
                        if (++p1 >= end1)
                        {
                            p1 = i;
                            p2 = i + l;
                        }
                        else
                            p2++;
                    }
                }

                X = tmp;
            }

            return X;
        }
        /// <summary>
        /// Возвращает спектр сигнала, w теперь считается в 2 раза меньше (w[i] = - w[i + N / 2])
        /// </summary>
        /// <param name="x">Массив значений сигнала. Количество значений должно быть степенью 2</param>
        /// <param name="direct">прямое или обратное преобразование</param>
        /// <returns>Массив со значениями спектра сигнала</returns>
        private static Complex[] _fft3(Complex[] x, bool direct)
        {
            int N = x.Length;
            Complex[] X = new Complex[N];
            int k = 0; //длина в битах  = lg(2)(N)
            while ((1 << k) < N) k++;

            //вспомагательный массив перестановок
            int[] rev = new int[N];
            rev[0] = 0;

            int high1 = -1;
            for (int i = 1; i < N; i++)
            {
                if ((i & (i - 1)) == 0) // Проверка на степень двойки. Если i ей является, то i-1 будет состоять из кучи единиц.
                    high1++;
                rev[i] = rev[i ^ (1 << high1)]; // Переворачиваем остаток
                rev[i] |= (1 << (k - high1 - 1)); // Добавляем старший бит
            }

            for (int i = 0; i < N; i++)
                X[i] = x[rev[i]];

            //сейчас в X находятся те же коеффициенты х, но в другом порядке 

            //l - количество элементов в блоке, при каждой итерации размер увеличивается в 2 раза
            for (int l = 1; l < N; l <<= 1)
            {
                Complex[] tmp = new Complex[N];//временный массив преобразований
                int l2 = l << 1; //= l * 2

                for (int i = 0; i < N; i += l2)
                {
                    int p1 = i,//начало первого блока
                        p2 = i + l,//начало второго блока
                        end1 = p2;//конец первого блока

                    for (int j = 0; j < l; j++)
                    {
                        int p3 = i + j;
                        Complex W = w(j, l2, direct) * X[p2];
                        tmp[p3] = X[p1] + W;
                        tmp[p3 + l] = X[p1] - W;
                        if (++p1 >= end1)
                        {
                            p1 = i;
                            p2 = i + l;
                        }
                        else
                            p2++;
                    }
                }

                X = tmp;
            }

            return X;
        }
        #endregion

        #region Публичные методы
        /// <summary>
        /// Прямое преобразование Фурье
        /// </summary>
        /// <param name="x">Массив значений сигнала. Количество значений должно быть степенью 2</param>
        /// <param name="method">Метод FFT   0 - с рекурсией, 1 - без рекурсии, 2 - с оптимизацией подсчета W</param>
        /// <returns></returns>
        public static Complex[] fft(Complex[] x, int method)
        {
            switch (method)
            {
                case 0:
                    return _fft(x, true);
                    break;
                case 1:
                    return _fft2(x, true);
                    break;
                case 2:
                    return _fft3(x, true);
                    break;
                default:
                    return null;
            }
        }

        /// <summary>
        /// Обратное преобразование Фурье (inverse)
        /// </summary>
        /// <param name="x">Массив значений сигнала. Количество значений должно быть степенью 2</param>
        /// <param name="method">Метод FFT   0 - с рекурсией, 1 - без рекурсии, 2 - с оптимизацией подсчета W</param>
        /// <returns></returns>
        public static Complex[] ifft(Complex[] x, int method)
        {
            Complex[] res;
            switch (method)
            {
                case 0:
                    res = _fft(x, false);
                    break;
                case 1:
                    res = _fft2(x, false);
                    break;
                case 2:
                    res = _fft3(x, false);
                    break;
                default:
                    return null;
            }
            for (int i = 0; i < res.Length; i++)
            {
                res[i] /= res.Length;
            }
            return res;
        }
        #endregion
    }
}