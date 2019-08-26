#ifndef __BIT7CHARS_H
#define __BIT7CHARS_H

#define B7_NULL "\x00"
#define B7_SMILE "\x01"
#define B7_OH "\x02"
#define B7_FROWN "\x03"
#define B7_HEART "\x04"
#define B7_DIAMOND "\x05"
#define B7_CIRCLE "\x06"
#define B7_BULLET "\x07"
#define B7_UNCHECKED "\x08"
#define B7_CHECKED "\x09"
// Line drawing. These are given by presence of
// the North, East, South and West directions in
// that order. Characters exist when at least two
// directions are set.
#define B7_NSW "\x0A"
#define B7_NES "\x0B"
#define B7_ESW "\x0C"
#define B7_NEW "\x0D"
#define B7_NESW "\x0E"
#define B7_NS "\x0F"
#define B7_NW "\x10"
#define B7_ES "\x11"
#define B7_SW "\x12"
#define B7_NE "\x13"
#define B7_EW "\x14"
#define B7_HALF_TOP "\x15"
#define B7_HALF_BOTTOM "\x16"
#define B7_HALF_LEFT "\x17"
#define B7_HALF_RIGHT "\x18"
// Shaded characters. FILL_7 is all bits set.
#define B7_FILL_7 "\x19"
#define B7_FILL_6 "\x1A"
#define B7_FILL_5 "\x1B"
#define B7_FILL_4 "\x1C"
#define B7_FILL_3 "\x1D"
#define B7_FILL_2 "\x1E"
#define B7_FILL_1 "\x1F"
// Alias for space.
#define B7_FILL_0 "\x20"
#define B7_NOTES "\x7F"

#endif
