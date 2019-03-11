/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.xlloop.util;

import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;

public class Day implements Comparable
{
    public static String[] MONTHS = { "Jan", "Feb", "Mar", "Apr", "May", "Jun",
            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };
    public static long MILLIS_IN_DAY = 86400000l;

    private int year;
    private int month;
    private int day;

    public Day(Date d) {
        GregorianCalendar gc = new GregorianCalendar();
        gc.setTime(d);
        year = gc.get(Calendar.YEAR);
        month = gc.get(Calendar.MONTH) + 1;
        day = gc.get(Calendar.DAY_OF_MONTH);
    }

    public Day() {
        GregorianCalendar gc = new GregorianCalendar();
        year = gc.get(Calendar.YEAR);
        month = gc.get(Calendar.MONTH) + 1;
        day = gc.get(Calendar.DAY_OF_MONTH);
    }

    public Day(int year, int month, int day) {
        this.year = year;
        this.month = month;
        this.day = day;
    }

    public int getYear() {
        return year;
    }

    public int getMonth() {
        return month;
    }

    public String getMonthString() {
        return MONTHS[month - 1];
    }

    public int getDay() {
        return day;
    }

    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + day;
        result = prime * result + month;
        result = prime * result + year;
        return result;
    }

    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        Day other = (Day) obj;
        if (day != other.day)
            return false;
        if (month != other.month)
            return false;
        if (year != other.year)
            return false;
        return true;
    }

    public int compareTo(Object o) {
        if (this == o)
            return 0;
        if (o == null)
            return 0;
        if (o.getClass() != getClass())
            return 0;
        Day d = (Day) o;
        int v = year - d.year;
        if (v != 0)
            return v;
        v = month - d.month;
        if (v != 0)
            return v;
        return day - d.day;
    }

    public String toString() {
        return day + "-" + getMonthString() + "-" + year;
    }

    public double toExcelDate() {
        return ExcelDate.date(year, month, day);
    }

    public static Day fromExcelDate(double date) {
        return new Day(ExcelDate.getYear(date), ExcelDate.getMonth(date),
                ExcelDate.getDayOfMonth(date));
    }
}
