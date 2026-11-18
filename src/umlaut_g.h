/*
 * umlaut.h
 * --------
 *
 * ISO Latin-1 umlauts for "phonet.c", "gender.c" and "addr.c".
 *
 * Copyright (c):
 * 2007:  Joerg MICHAEL, Adalbert-Stifter-Str. 11, 30655 Hannover, Germany
 *
 * SCCS: @(#) umlaut.h  1.0  2007-08-27
 *
 * This program is subject to the GNU Lesser General Public License (LGPL)
 * (formerly known as GNU Library General Public Licence)
 * as published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * You should have received a copy of the GNU Library General Public License
 * along with this program; if not, write to the
 * Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Actually, the LGPL is __less__ restrictive than the better known GNU General
 * Public License (GPL). See the GNU Library General Public License or the file
 * LIB_GPLG.TXT for more details and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * There is one important restriction: If you modify this program in any way
 * (e.g. add or change phonetic rules or modify the underlying logic or
 * translate this program into another programming language), you must also
 * release the changes under the terms of the LGPL.
 * (However, since __this__ program is intended to be customized, all changes
 * covered by TO-DO comments are free.)
 *
 * That means you have to give out the source code to your changes,
 * and a very good way to do so is mailing them to the address given below.
 * I think this is the best way to promote further development and use
 * of this software.
 *
 * If you have any remarks, feel free to e-mail to:
 *     ct@ct.heise.de
 *
 * The author's email address is:
 *    astro.joerg@googlemail.com
 */


#ifndef _UMLAUT_AGP_H_
#define _UMLAUT_AGP_H_


/****  list of "normal" letters and umlauts, with upper case  ****/
/****  (char set = iso8859-1) - DO NOT CHANGE THESE MACROS    ****/
#define letters_a_to_z   "abcdefghijklmnopqrstuvwxyz"
#define letters_A_to_Z   "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
#define umlaut_lower   "\340\341\342\343\345\344\346\347\360\350\351\352\353\354\355\356\357\361\362\363\364\365\366\370\234\232\337\376\371\372\373\374\375\377"
#define umlaut_upper   "\300\301\302\303\305\304\306\307\320\310\311\312\313\314\315\316\317\321\322\323\324\325\326\330\214\212\337\336\331\332\333\334\335\237"

/****  macro for "lev100.h"  ****/
#define umlaut_conv    "AAAAA\304\304CDEEEEIIIINOOOO\326\326\326S\337\336UUU\334YY"
/****  macros for "gender.c"  ****/
#define umlaut_sort    "AAAAAAACDEEEEIIIINOOOOOOOSSTUUUUYY"
#define umlaut_sort2   "    AEE          H    EEE SH   E  "



/****  TO-DO:  If you want to convert DOS umlauts to iso chars,   ****/
/****     use the following macros to create a translation table  ****/
/**
 #define DOS_chars  "???????????????????????????????????????????????????????????????"
 #define iso_chars  "???????????????????????????????????????????????????????????????"
**/


/****  TO-DO:  If you want to convert Mac umlauts to iso chars,   ****/
/****     use the following macros to create a translation table  ****/
/**
 #define Mac_chars  "???????????????????????????????????????????????????????????"
 #define iso_chars  "???????????????????????????????????????????????????????????"
**/

#endif     /****  _UMLAUT_AGP_H_  ****/

/************************************************************/
/****  end of file "umlaut.h"  ******************************/
/************************************************************/
