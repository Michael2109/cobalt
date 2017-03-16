/*
 * Cobalt Programming Language Compiler
 * Copyright (C) 2017  Cobalt
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package test_classes;

/**
 * Used to view ASM code
 */
public class TestCode {

    void test() {
        int x = 1;
        float y = 2;
        double z = 3;
        short s = 4;
        long l = 50000000000000L;
        byte b = 10;
        System.out.println(x);
        System.out.println(y);
        System.out.println(z);
        System.out.println(s);
        System.out.println(l);
        System.out.println(b);
    }

}
