/*
 * Diff (without the Match and Patch)
 * Copyright 2018 The diff-match-patch Authors.
 * Copyright 2019 Victor Grishchenko
 * https://github.com/google/diff-match-patch
 * https://github.com/gritzko/myers-diff
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
#ifndef MyersDiff_HPP
#define MyersDiff_HPP

#include <assert.h>
#include <limits.h>
#include <stdint.h>
#include <string>
#include <algorithm>
#include <chrono>
#include <unordered_map>
#include <vector>

using namespace std;

enum Operation : int8_t { EQUAL=0, INSERT=1, DELETE=2 };

inline char op2chr(Operation op) {
    switch (op) {
        case DELETE:
            return '-';
        case INSERT:
            return '+';
        case EQUAL:
            return '=';
        default:
            return '?';
    }
}

/*
 * Computes the difference between two texts to create a patch.
 * Also contains the behaviour settings.
 */
template <class String>
class MyersDiff {
   public:
    // Defaults.
    // Set these on your diff_match_patch instance to override the defaults.

    /**
     * Number of milliseconds to map a diff before giving up (0 for infinity).
     */
    long Diff_Timeout = 1000;
    /**
     * Cost of an empty edit operation in terms of edit characters.
     */
    uint16_t Diff_EditCost = 4;
    /**
     * At what point is no match declared (0.0 = perfection, 1.0 = very loose).
     */
    float Match_Threshold = 0.5f;
    /**
     * How far to search for a match (0 = exact location, 1000+ = broad match).
     * A match this many characters away from the expected location will add
     * 1.0 to the score (0.0 is a perfect match).
     */
    int Match_Distance = 1000;
    /**
     * When deleting a large block of text (over ~64 characters), how close do
     * the contents have to be to match the expected contents. (0.0 =
     * perfection, 1.0 = very loose).  Note that Match_Threshold controls how
     * closely the end points of a delete need to match.
     */
    float Patch_DeleteThreshold = 0.5f;
    /**
     * Chunk size for context length.
     */
    uint16_t Patch_Margin = 4;

   public:
    using Char = typename String::value_type;
    using Time = chrono::time_point<chrono::steady_clock>;
    using Size = typename String::size_type;
    using ConstIter = typename String::const_iterator;

    struct Range {
        ConstIter from, till;
        Range(ConstIter begin, ConstIter end) : from{begin}, till{end} {}
        explicit Range(const String& str)
            : from{str.begin()}, till{str.end()} {}
        bool operator==(Range b) const {
            if (till - from != b.till - b.from) return false;
            for (auto i = from, j = b.from; i < till; ++i, ++j)
                if (*i != *j) return false;
            return true;
        }
        Size size() const { return till - from; }
        Range substr(Size start) const {
            assert(start <= size());
            return Range{from + start, till};
        }
        Range substr(Size start, Size end) const {
            assert(end >= start);
            assert(start <= size());
            if (end >= size()) end = size();
            return Range{from + start, from + end};
        }
        Char operator[](Size idx) const {
            assert(idx < size());
            return *(from + idx);
        }
        Size find(Range b) const {
            auto at = std::search(from, till, b.from, b.till);
            return at < till ? at - from : -1;
        }
    };

    struct Diff {
        Operation operation;
        Range text;
        Diff(Operation op, Range text_range)
            : operation{op}, text{text_range} {}
        std::string str() const {
            string ret;
            ret.push_back(op2chr(operation));
            ret.push_back('\t');
            ret.append(text.from, text.till);
            return ret;
        }
    };

    using Diffs = std::vector<Diff>;

   private:
    const String& text1;
    const String& text2;
    Diffs result;

   public:
    MyersDiff(const String& original_text, const String& changed_text)
        : text1{original_text}, text2{changed_text} {
        result = diff_main(Range{text1}, Range{text2});
    }

    typename Diffs::const_iterator begin() const { return result.begin(); }
    typename Diffs::const_iterator end() const { return result.end(); }

    const Diffs& diffs() const { return result; }

    //  DIFF FUNCTIONS

    /**
     * Find the differences between two texts.
     * @return std::vector of Diff objects.
     */
    Diffs diff_main(Range text1, Range text2) {
        // Set a deadline by which time the diff must be complete.
        Time deadline;
        if (Diff_Timeout <= 0) {
            deadline = Time::max();
        } else {
            deadline = chrono::steady_clock::now() +
                       chrono::milliseconds(Diff_Timeout);
        }
        return diff_main(text1, text2, deadline);
    }

    /**
     * Find the differences between two texts.  Simplifies the problem by
     * stripping any common prefix or suffix off the texts before diffing.
     * @param deadline Time when the diff should be complete by.  Used
     *     internally for recursive calls.  Users should set DiffTimeout
     * instead.
     * @return std::vector of Diff objects.
     */
    Diffs diff_main(Range text1, Range text2, Time deadline) {
        // Check for equality (speedup).
        Diffs diffs{};
        if (text1 == text2) {
            if (text1.size() != 0) {
                diffs.push_back(Diff(EQUAL, text1));
            }
            return diffs;
        }

        // Trim off common prefix (speedup).
        int commonlength = diff_commonPrefix(text1, text2);
        Range commonprefix = text1.substr(0, commonlength);
        text1 = text1.substr(commonlength);
        text2 = text2.substr(commonlength);

        // Trim off common suffix (speedup).
        commonlength = diff_commonSuffix(text1, text2);
        Range commonsuffix = text1.substr(text1.size() - commonlength);
        text1 = text1.substr(0, text1.size() - commonlength);
        text2 = text2.substr(0, text2.size() - commonlength);

        // Compute the diff on the middle block.
        diffs = diff_compute(text1, text2, deadline);

        // Restore the prefix and suffix.
        if (commonprefix.size() != 0) {
            diffs.insert(diffs.begin(), Diff(EQUAL, commonprefix));
        }
        if (commonsuffix.size() != 0) {
            diffs.push_back(Diff(EQUAL, commonsuffix));
        }

        // TODO diff_cleanupMerge(diffs);
        return diffs;
    }

    /**
     * Find the differences between two texts.  Assumes that the texts do not
     * have any common prefix or suffix.
     * @param text1 Old string to be diffed.
     * @param text2 New string to be diffed.
     * @param checklines Speedup flag.  If false, then don't run a
     *     line-level diff first to identify the changed areas.
     *     If true, then run a faster slightly less optimal diff.
     * @param deadline Time when the diff should be complete by.
     * @return std::vector of Diff objects.
     */
    Diffs diff_compute(Range text1, Range text2, Time deadline) {
        Diffs diffs{};

        if (text1.size() == 0) {
            // Just add some text (speedup).
            diffs.push_back(Diff(INSERT, text2));
            return diffs;
        }

        if (text2.size() == 0) {
            // Just delete some text (speedup).
            diffs.push_back(Diff(DELETE, text1));
            return diffs;
        }

        Range longtext = text1.size() > text2.size() ? text1 : text2;
        Range shorttext = text1.size() > text2.size() ? text2 : text1;
        int i = longtext.find(shorttext);
        if (i != -1) {
            // Shorter text is inside the longer text (speedup).
            Operation op = (text1.size() > text2.size()) ? DELETE : INSERT;
            diffs.push_back(Diff(op, longtext.substr(0, i)));
            diffs.push_back(Diff(EQUAL, shorttext));
            diffs.push_back(Diff(op, longtext.substr(i + shorttext.size())));
            return diffs;
        }

        if (shorttext.size() == 1) {
            // Single character string.
            // After the previous speedup, the character can't be an equality.
            diffs.push_back(Diff(DELETE, text1));
            diffs.push_back(Diff(INSERT, text2));
            return diffs;
        }

        // Check to see if the problem can be split in two.
        /* TODO String[] hm = diff_halfMatch(text1, text2);
        if (hm != null) {
          // A half-match was found, sort out the return data.
          Range text1_a = hm[0];
          Range text1_b = hm[1];
          Range text2_a = hm[2];
          Range text2_b = hm[3];
          String mid_common = hm[4];
          // Send both pairs off for separate processing.
          Diffs diffs_a = diff_main(text1_a, text2_a,
                                               checklines, deadline);
          Diffs diffs_b = diff_main(text1_b, text2_b,
                                               checklines, deadline);
          // Merge the results.
          diffs = diffs_a;
          diffs.push_back(Diff(EQUAL, mid_common));
          diffs.addAll(diffs_b);
          return diffs;
        }

        if (checklines && text1.size() > 100 && text2.size() > 100) {
          return diff_lineMode(text1, text2, deadline);
        }*/

        return diff_bisect(text1, text2, deadline);
    }

    /**
     * Find the 'middle snake' of a diff, split the problem in two
     * and return the recursively constructed diff.
     * See Myers 1986 paper: An O(ND) Difference Algorithm and Its Variations.
     * @param text1 Old string to be diffed.
     * @param text2 New string to be diffed.
     * @param deadline Time at which to bail if not yet complete.
     * @return std::vector of Diff objects.
     */
    Diffs diff_bisect(Range text1, Range text2, Time deadline) {
        // Cache the text lengths to prevent multiple calls.
        int text1_length = text1.size();
        int text2_length = text2.size();
        int max_d = (text1_length + text2_length + 1) / 2;
        int v_offset = max_d;
        int v_length = 2 * max_d;
        vector<int> v1;
        v1.resize(v_length);
        vector<int> v2;
        v2.resize(v_length);
        for (int x = 0; x < v_length; x++) {
            v1[x] = -1;
            v2[x] = -1;
        }
        v1[v_offset + 1] = 0;
        v2[v_offset + 1] = 0;
        int delta = text1_length - text2_length;
        // If the total number of characters is odd, then the front path will
        // collide with the reverse path.
        bool front = (delta % 2 != 0);
        // Offsets for start and end of k loop.
        // Prevents mapping of space beyond the grid.
        int k1start = 0;
        int k1end = 0;
        int k2start = 0;
        int k2end = 0;
        for (int d = 0; d < max_d; d++) {
            // Bail out if deadline is reached.
            /* TODO if (System.currentTimeMillis() > deadline) {
              break;
            } */

            // Walk the front path one step.
            for (int k1 = -d + k1start; k1 <= d - k1end; k1 += 2) {
                int k1_offset = v_offset + k1;
                int x1;
                if (k1 == -d ||
                    (k1 != d && v1[k1_offset - 1] < v1[k1_offset + 1])) {
                    x1 = v1[k1_offset + 1];
                } else {
                    x1 = v1[k1_offset - 1] + 1;
                }
                int y1 = x1 - k1;
                while (x1 < text1_length && y1 < text2_length &&
                       text1[x1] == text2[y1]) {
                    x1++;
                    y1++;
                }
                v1[k1_offset] = x1;
                if (x1 > text1_length) {
                    // Ran off the right of the graph.
                    k1end += 2;
                } else if (y1 > text2_length) {
                    // Ran off the bottom of the graph.
                    k1start += 2;
                } else if (front) {
                    int k2_offset = v_offset + delta - k1;
                    if (k2_offset >= 0 && k2_offset < v_length &&
                        v2[k2_offset] != -1) {
                        // Mirror x2 onto top-left coordinate system.
                        int x2 = text1_length - v2[k2_offset];
                        if (x1 >= x2) {
                            // Overlap detected.
                            return diff_bisectSplit(text1, text2, x1, y1,
                                                    deadline);
                        }
                    }
                }
            }

            // Walk the reverse path one step.
            for (int k2 = -d + k2start; k2 <= d - k2end; k2 += 2) {
                int k2_offset = v_offset + k2;
                int x2;
                if (k2 == -d ||
                    (k2 != d && v2[k2_offset - 1] < v2[k2_offset + 1])) {
                    x2 = v2[k2_offset + 1];
                } else {
                    x2 = v2[k2_offset - 1] + 1;
                }
                int y2 = x2 - k2;
                while (x2 < text1_length && y2 < text2_length &&
                       text1[text1_length - x2 - 1] ==
                           text2[text2_length - y2 - 1]) {
                    x2++;
                    y2++;
                }
                v2[k2_offset] = x2;
                if (x2 > text1_length) {
                    // Ran off the left of the graph.
                    k2end += 2;
                } else if (y2 > text2_length) {
                    // Ran off the top of the graph.
                    k2start += 2;
                } else if (!front) {
                    int k1_offset = v_offset + delta - k2;
                    if (k1_offset >= 0 && k1_offset < v_length &&
                        v1[k1_offset] != -1) {
                        int x1 = v1[k1_offset];
                        int y1 = v_offset + x1 - k1_offset;
                        // Mirror x2 onto top-left coordinate system.
                        x2 = text1_length - x2;
                        if (x1 >= x2) {
                            // Overlap detected.
                            return diff_bisectSplit(text1, text2, x1, y1,
                                                    deadline);
                        }
                    }
                }
            }
        }
        // Diff took too long and hit the deadline or
        // number of diffs equals number of characters, no commonality at all.
        Diffs diffs{};
        diffs.push_back(Diff{DELETE, text1});
        diffs.push_back(Diff{INSERT, text2});
        return diffs;
    }

    /**
     * Given the location of the 'middle snake', split the diff in two parts
     * and recurse.
     * @param text1 Old string to be diffed.
     * @param text2 New string to be diffed.
     * @param x Index of split point in text1.
     * @param y Index of split point in text2.
     * @param deadline Time at which to bail if not yet complete.
     * @return std::vector of Diff objects.
     */
    Diffs diff_bisectSplit(Range text1, Range text2, int x, int y,
                           Time deadline) {
        Range text1a = text1.substr(0, x);
        Range text2a = text2.substr(0, y);
        Range text1b = text1.substr(x);
        Range text2b = text2.substr(y);

        // Compute both diffs serially.
        Diffs diffs = diff_main(text1a, text2a, deadline);
        Diffs diffsb = diff_main(text1b, text2b, deadline);

        diffs.insert(diffs.end(), diffsb.begin(), diffsb.end());
        return diffs;
    }

    /**
     * Determine the common prefix of two strings
     * @param text1 First string.
     * @param text2 Second string.
     * @return The number of characters common to the start of each string.
     */
    int diff_commonPrefix(Range text1, Range text2) {
        // Performance analysis: https://neil.fraser.name/news/2007/10/09/
        int n = std::min(text1.size(), text2.size());
        for (int i = 0; i < n; i++) {
            if (text1[i] != text2[i]) {
                return i;
            }
        }
        return n;
    }

    /**
     * Determine the common suffix of two strings
     * @param text1 First string.
     * @param text2 Second string.
     * @return The number of characters common to the end of each string.
     */
    int diff_commonSuffix(Range text1, Range text2) {
        // Performance analysis: https://neil.fraser.name/news/2007/10/09/
        int text1_length = text1.size();
        int text2_length = text2.size();
        int n = std::min(text1_length, text2_length);
        for (int i = 1; i <= n; i++) {
            if (text1[text1_length - i] != text2[text2_length - i]) {
                return i - 1;
            }
        }
        return n;
    }

    /**
     * Determine if the suffix of one string is the prefix of another.
     * @param text1 First string.
     * @param text2 Second string.
     * @return The number of characters common to the end of the first
     *     string and the start of the second string.
     */
    int diff_commonOverlap(Range text1, Range text2) {
        // Cache the text lengths to prevent multiple calls.
        int text1_length = text1.size();
        int text2_length = text2.size();
        // Eliminate the null case.
        if (text1_length == 0 || text2_length == 0) {
            return 0;
        }
        // Truncate the longer string.
        if (text1_length > text2_length) {
            text1 = text1.substr(text1_length - text2_length);
        } else if (text1_length < text2_length) {
            text2 = text2.substr(0, text1_length);
        }
        int text_length = std::min(text1_length, text2_length);
        // Quick check for the worst case.
        if (text1 == text2) {
            return text_length;
        }

        // Start by looking for a single character match
        // and increase length until no match is found.
        // Performance analysis: https://neil.fraser.name/news/2010/11/04/
        int best = 0;
        int length = 1;
        while (true) {
            String pattern = text1.substr(text_length - length);
            int found = text2.indexOf(pattern);
            if (found == -1) {
                return best;
            }
            length += found;
            if (found == 0 ||
                text1.substr(text_length - length) == text2.substr(0, length)) {
                best = length;
                length++;
            }
        }
    }

    /**
     * Do the two texts share a substring which is at least half the length of
     * the longer text?
     * This speedup can produce non-minimal diffs.
     * @param text1 First string.
     * @param text2 Second string.
     * @return Five element String array, containing the prefix of text1, the
     *     suffix of text1, the prefix of text2, the suffix of text2 and the
     *     common middle.  Or null if there was no match.
     */
    /*  TODO
   String[] diff_halfMatch(Range text1, Range text2) {
     if (Diff_Timeout <= 0) {
       // Don't risk returning a non-optimal diff if we have unlimited time.
       return null;
     }
     String longtext = text1.size() > text2.size() ? text1 : text2;
     String shorttext = text1.size() > text2.size() ? text2 : text1;
     if (longtext.size() < 4 || shorttext.size() * 2 < longtext.size()) {
       return null;  // Pointless.
     }

     // First check if the second quarter is the seed for a half-match.
     String[] hm1 = diff_halfMatchI(longtext, shorttext,
                                    (longtext.size() + 3) / 4);
     // Check again based on the third quarter.
     String[] hm2 = diff_halfMatchI(longtext, shorttext,
                                    (longtext.size() + 1) / 2);
     String[] hm;
     if (hm1 == null && hm2 == null) {
       return null;
     } else if (hm2 == null) {
       hm = hm1;
     } else if (hm1 == null) {
       hm = hm2;
     } else {
       // Both matched.  Select the longest.
       hm = hm1[4].size() > hm2[4].size() ? hm1 : hm2;
     }

     // A half-match was found, sort out the return data.
     if (text1.size() > text2.size()) {
       return hm;
       //return new String[]{hm[0], hm[1], hm[2], hm[3], hm[4]};
     } else {
       return new String[]{hm[2], hm[3], hm[0], hm[1], hm[4]};
     }
   }
   */

    /**
     * Does a substring of shorttext exist within longtext such that the
     * substring is at least half the length of longtext?
     * @param longtext Longer string.
     * @param shorttext Shorter string.
     * @param i Start index of quarter length substring within longtext.
     * @return Five element String array, containing the prefix of longtext, the
     *     suffix of longtext, the prefix of shorttext, the suffix of shorttext
     *     and the common middle.  Or null if there was no match.
     *
    String[] diff_halfMatchI(String longtext, String shorttext, int i) {
      // Start with a 1/4 length substring at position i as a seed.
      String seed = longtext.substring(i, i + longtext.size() / 4);
      int j = -1;
      String best_common = "";
      String best_longtext_a = "", best_longtext_b = "";
      String best_shorttext_a = "", best_shorttext_b = "";
      while ((j = shorttext.indexOf(seed, j + 1)) != -1) {
        int prefixLength = diff_commonPrefix(longtext.substring(i),
                                             shorttext.substring(j));
        int suffixLength = diff_commonSuffix(longtext.substring(0, i),
                                             shorttext.substring(0, j));
        if (best_common.size() < suffixLength + prefixLength) {
          best_common = shorttext.substring(j - suffixLength, j)
              + shorttext.substring(j, j + prefixLength);
          best_longtext_a = longtext.substring(0, i - suffixLength);
          best_longtext_b = longtext.substring(i + prefixLength);
          best_shorttext_a = shorttext.substring(0, j - suffixLength);
          best_shorttext_b = shorttext.substring(j + prefixLength);
        }
      }
      if (best_common.size() * 2 >= longtext.size()) {
        return new String[]{best_longtext_a, best_longtext_b,
                            best_shorttext_a, best_shorttext_b, best_common};
      } else {
        return null;
      }
    }
    */

    /**
     * Reduce the number of edits by eliminating semantically trivial
    equalities.
     * @param diffs std::vector of Diff objects.
     *
    void diff_cleanupSemantic(Diffs diffs) {
      if (diffs.isEmpty()) {
        return;
      }
      bool changes = false;
      std::deque<Diff> equalities = new ArrayDeque<Diff>();  // Double-ended
    queue of qualities. String lastEquality = null; // Always equal to
    equalities.peek().text std::vectorIterator<Diff> pointer =
    diffs.listIterator();
      // Number of characters that changed prior to the equality.
      int length_insertions1 = 0;
      int length_deletions1 = 0;
      // Number of characters that changed after the equality.
      int length_insertions2 = 0;
      int length_deletions2 = 0;
      Diff thisDiff = pointer.next();
      while (thisDiff != null) {
        if (thisDiff.operation == EQUAL) {
          // Equality found.
          equalities.push(thisDiff);
          length_insertions1 = length_insertions2;
          length_deletions1 = length_deletions2;
          length_insertions2 = 0;
          length_deletions2 = 0;
          lastEquality = thisDiff.text;
        } else {
          // An insertion or deletion.
          if (thisDiff.operation == INSERT) {
            length_insertions2 += thisDiff.text.size();
          } else {
            length_deletions2 += thisDiff.text.size();
          }
          // Eliminate an equality that is smaller or equal to the edits on both
          // sides of it.
          if (lastEquality != null && (lastEquality.size()
              <= std::max(length_insertions1, length_deletions1))
              && (lastEquality.size()
                  <= std::max(length_insertions2, length_deletions2))) {
            //System.out.println("Splitting: '" + lastEquality + "'");
            // Walk back to offending equality.
            while (thisDiff != equalities.peek()) {
              thisDiff = pointer.previous();
            }
            pointer.next();

            // Replace equality with a delete.
            pointer.set(Diff(DELETE, lastEquality));
            // Insert a corresponding an insert.
            pointer.push_back(Diff(INSERT, lastEquality));

            equalities.pop();  // Throw away the equality we just deleted.
            if (!equalities.isEmpty()) {
              // Throw away the previous equality (it needs to be reevaluated).
              equalities.pop();
            }
            if (equalities.isEmpty()) {
              // There are no previous equalities, walk back to the start.
              while (pointer.hasPrevious()) {
                pointer.previous();
              }
            } else {
              // There is a safe equality we can fall back to.
              thisDiff = equalities.peek();
              while (thisDiff != pointer.previous()) {
                // Intentionally empty loop.
              }
            }

            length_insertions1 = 0;  // Reset the counters.
            length_insertions2 = 0;
            length_deletions1 = 0;
            length_deletions2 = 0;
            lastEquality = null;
            changes = true;
          }
        }
        thisDiff = pointer.hasNext() ? pointer.next() : null;
      }

      // Normalize the diff.
      if (changes) {
        diff_cleanupMerge(diffs);
      }
      diff_cleanupSemanticLossless(diffs);

      // Find any overlaps between deletions and insertions.
      // e.g: <del>abcxxx</del><ins>xxxdef</ins>
      //   -> <del>abc</del>xxx<ins>def</ins>
      // e.g: <del>xxxabc</del><ins>defxxx</ins>
      //   -> <ins>def</ins>xxx<del>abc</del>
      // Only extract an overlap if it is as big as the edit ahead or behind it.
      pointer = diffs.listIterator();
      Diff prevDiff = null;
      thisDiff = null;
      if (pointer.hasNext()) {
        prevDiff = pointer.next();
        if (pointer.hasNext()) {
          thisDiff = pointer.next();
        }
      }
      while (thisDiff != null) {
        if (prevDiff.operation == DELETE &&
            thisDiff.operation == INSERT) {
          String deletion = prevDiff.text;
          String insertion = thisDiff.text;
          int overlap_length1 = diff_commonOverlap(deletion, insertion);
          int overlap_length2 = diff_commonOverlap(insertion, deletion);
          if (overlap_length1 >= overlap_length2) {
            if (overlap_length1 >= deletion.size() / 2.0 ||
                overlap_length1 >= insertion.size() / 2.0) {
              // Overlap found. Insert an equality and trim the surrounding
    edits. pointer.previous(); pointer.push_back(Diff(EQUAL,
                                   insertion.substring(0, overlap_length1)));
              prevDiff.text =
                  deletion.substring(0, deletion.size() - overlap_length1);
              thisDiff.text = insertion.substring(overlap_length1);
              // pointer.add inserts the element before the cursor, so there is
              // no need to step past the new element.
            }
          } else {
            if (overlap_length2 >= deletion.size() / 2.0 ||
                overlap_length2 >= insertion.size() / 2.0) {
              // Reverse overlap found.
              // Insert an equality and swap and trim the surrounding edits.
              pointer.previous();
              pointer.push_back(Diff(EQUAL,
                                   deletion.substring(0, overlap_length2)));
              prevDiff.operation = INSERT;
              prevDiff.text =
                insertion.substring(0, insertion.size() - overlap_length2);
              thisDiff.operation = DELETE;
              thisDiff.text = deletion.substring(overlap_length2);
              // pointer.add inserts the element before the cursor, so there is
              // no need to step past the new element.
            }
          }
          thisDiff = pointer.hasNext() ? pointer.next() : null;
        }
        prevDiff = thisDiff;
        thisDiff = pointer.hasNext() ? pointer.next() : null;
      }
    }
    */

    /**
     * Look for single edits surrounded on both sides by equalities
     * which can be shifted sideways to align the edit to a word boundary.
     * e.g: The c<ins>at c</ins>ame. -> The <ins>cat </ins>came.
     * @param diffs std::vector of Diff objects.
     * TODO
    void diff_cleanupSemanticLossless(Diffs diffs) {
      String equality1, edit, equality2;
      String commonString;
      int commonOffset;
      int score, bestScore;
      String bestEquality1, bestEdit, bestEquality2;
      // Create a new iterator at the start.
      std::vectorIterator<Diff> pointer = diffs.listIterator();
      Diff prevDiff = pointer.hasNext() ? pointer.next() : null;
      Diff thisDiff = pointer.hasNext() ? pointer.next() : null;
      Diff nextDiff = pointer.hasNext() ? pointer.next() : null;
      // Intentionally ignore the first and last element (don't need checking).
      while (nextDiff != null) {
        if (prevDiff.operation == EQUAL &&
            nextDiff.operation == EQUAL) {
          // This is a single edit surrounded by equalities.
          equality1 = prevDiff.text;
          edit = thisDiff.text;
          equality2 = nextDiff.text;

          // First, shift the edit as far left as possible.
          commonOffset = diff_commonSuffix(equality1, edit);
          if (commonOffset != 0) {
            commonString = edit.substring(edit.size() - commonOffset);
            equality1 = equality1.substring(0, equality1.size() -
    commonOffset); edit = commonString + edit.substring(0, edit.size() -
    commonOffset); equality2 = commonString + equality2;
          }

          // Second, step character by character right, looking for the best
    fit. bestEquality1 = equality1; bestEdit = edit; bestEquality2 = equality2;
          bestScore = diff_cleanupSemanticScore(equality1, edit)
              + diff_cleanupSemanticScore(edit, equality2);
          while (edit.size() != 0 && equality2.size() != 0
              && edit.charAt(0) == equality2.charAt(0)) {
            equality1 += edit.charAt(0);
            edit = edit.substring(1) + equality2.charAt(0);
            equality2 = equality2.substring(1);
            score = diff_cleanupSemanticScore(equality1, edit)
                + diff_cleanupSemanticScore(edit, equality2);
            // The >= encourages trailing rather than leading whitespace on
    edits. if (score >= bestScore) { bestScore = score; bestEquality1 =
    equality1; bestEdit = edit; bestEquality2 = equality2;
            }
          }

          if (!prevDiff.text.equals(bestEquality1)) {
            // We have an improvement, save it back to the diff.
            if (bestEquality1.size() != 0) {
              prevDiff.text = bestEquality1;
            } else {
              pointer.previous(); // Walk past nextDiff.
              pointer.previous(); // Walk past thisDiff.
              pointer.previous(); // Walk past prevDiff.
              pointer.remove(); // Delete prevDiff.
              pointer.next(); // Walk past thisDiff.
              pointer.next(); // Walk past nextDiff.
            }
            thisDiff.text = bestEdit;
            if (bestEquality2.size() != 0) {
              nextDiff.text = bestEquality2;
            } else {
              pointer.remove(); // Delete nextDiff.
              nextDiff = thisDiff;
              thisDiff = prevDiff;
            }
          }
        }
        prevDiff = thisDiff;
        thisDiff = nextDiff;
        nextDiff = pointer.hasNext() ? pointer.next() : null;
      }
    }
    */

    /**
     * Given two strings, compute a score representing whether the internal
     * boundary falls on logical boundaries.
     * Scores range from 6 (best) to 0 (worst).
     * @param one First string.
     * @param two Second string.
     * @return The score.
     * TODO
    int diff_cleanupSemanticScore(String one, String two) {
      if (one.size() == 0 || two.size() == 0) {
        // Edges are the best.
        return 6;
      }

      // Each port of this function behaves slightly differently due to
      // subtle differences in each language's definition of things like
      // 'whitespace'.  Since this function's purpose is largely cosmetic,
      // the choice has been made to use each language's native features
      // rather than force total conformity.
      char char1 = one.charAt(one.size() - 1);
      char char2 = two.charAt(0);
      bool nonAlphaNumeric1 = !Character.isLetterOrDigit(char1);
      bool nonAlphaNumeric2 = !Character.isLetterOrDigit(char2);
      bool whitespace1 = nonAlphaNumeric1 && Character.isWhitespace(char1);
      bool whitespace2 = nonAlphaNumeric2 && Character.isWhitespace(char2);
      bool lineBreak1 = whitespace1
          && Character.getType(char1) == Character.CONTROL;
      bool lineBreak2 = whitespace2
          && Character.getType(char2) == Character.CONTROL;
      bool blankLine1 = lineBreak1 && BLANKLINEEND.matcher(one).find();
      bool blankLine2 = lineBreak2 && BLANKLINESTART.matcher(two).find();

      if (blankLine1 || blankLine2) {
        // Five points for blank lines.
        return 5;
      } else if (lineBreak1 || lineBreak2) {
        // Four points for line breaks.
        return 4;
      } else if (nonAlphaNumeric1 && !whitespace1 && whitespace2) {
        // Three points for end of sentences.
        return 3;
      } else if (whitespace1 || whitespace2) {
        // Two points for whitespace.
        return 2;
      } else if (nonAlphaNumeric1 || nonAlphaNumeric2) {
        // One point for non-alphanumeric.
        return 1;
      }
      return 0;
    }
    */

    /* Define some regex patterns for matching boundaries.
    Pattern BLANKLINEEND
        = Pattern.compile("\\n\\r?\\n\\Z", Pattern.DOTALL);
    Pattern BLANKLINESTART
        = Pattern.compile("\\A\\r?\\n\\r?\\n", Pattern.DOTALL);
        */

    /**
     * Reduce the number of edits by eliminating operationally trivial
    equalities.
     * @param diffs std::vector of Diff objects.
     * TODO
    void diff_cleanupEfficiency(Diffs diffs) {
      if (diffs.isEmpty()) {
        return;
      }
      bool changes = false;
      std::deque<Diff> equalities = new ArrayDeque<Diff>();  // Double-ended
    queue of equalities. String lastEquality = null; // Always equal to
    equalities.peek().text std::vectorIterator<Diff> pointer =
    diffs.listIterator();
      // Is there an insertion operation before the last equality.
      bool pre_ins = false;
      // Is there a deletion operation before the last equality.
      bool pre_del = false;
      // Is there an insertion operation after the last equality.
      bool post_ins = false;
      // Is there a deletion operation after the last equality.
      bool post_del = false;
      Diff thisDiff = pointer.next();
      Diff safeDiff = thisDiff;  // The last Diff that is known to be
    unsplittable. while (thisDiff != null) { if (thisDiff.operation == EQUAL) {
          // Equality found.
          if (thisDiff.text.size() < Diff_EditCost && (post_ins || post_del))
    {
            // Candidate found.
            equalities.push(thisDiff);
            pre_ins = post_ins;
            pre_del = post_del;
            lastEquality = thisDiff.text;
          } else {
            // Not a candidate, and can never become one.
            equalities.clear();
            lastEquality = null;
            safeDiff = thisDiff;
          }
          post_ins = post_del = false;
        } else {
          // An insertion or deletion.
          if (thisDiff.operation == DELETE) {
            post_del = true;
          } else {
            post_ins = true;
          }
          /*
           * Five types to be split:
           * <ins>A</ins><del>B</del>XY<ins>C</ins><del>D</del>
           * <ins>A</ins>X<ins>C</ins><del>D</del>
           * <ins>A</ins><del>B</del>X<ins>C</ins>
           * <ins>A</del>X<ins>C</ins><del>D</del>
           * <ins>A</ins><del>B</del>X<del>C</del>
           * /
          if (lastEquality != null
              && ((pre_ins && pre_del && post_ins && post_del)
                  || ((lastEquality.size() < Diff_EditCost / 2)
                      && ((pre_ins ? 1 : 0) + (pre_del ? 1 : 0)
                          + (post_ins ? 1 : 0) + (post_del ? 1 : 0)) == 3))) {
            //System.out.println("Splitting: '" + lastEquality + "'");
            // Walk back to offending equality.
            while (thisDiff != equalities.peek()) {
              thisDiff = pointer.previous();
            }
            pointer.next();

            // Replace equality with a delete.
            pointer.set(Diff(DELETE, lastEquality));
            // Insert a corresponding an insert.
            pointer.push_back(thisDiff = Diff(INSERT, lastEquality));

            equalities.pop();  // Throw away the equality we just deleted.
            lastEquality = null;
            if (pre_ins && pre_del) {
              // No changes made which could affect previous entry, keep going.
              post_ins = post_del = true;
              equalities.clear();
              safeDiff = thisDiff;
            } else {
              if (!equalities.isEmpty()) {
                // Throw away the previous equality (it needs to be
    reevaluated). equalities.pop();
              }
              if (equalities.isEmpty()) {
                // There are no previous questionable equalities,
                // walk back to the last known safe diff.
                thisDiff = safeDiff;
              } else {
                // There is an equality we can fall back to.
                thisDiff = equalities.peek();
              }
              while (thisDiff != pointer.previous()) {
                // Intentionally empty loop.
              }
              post_ins = post_del = false;
            }

            changes = true;
          }
        }
        thisDiff = pointer.hasNext() ? pointer.next() : null;
      }

      if (changes) {
        diff_cleanupMerge(diffs);
      }
    }
    */

    /**
     * Reorder and merge like edit sections.  Merge equalities.
     * Any edit section can move as long as it doesn't cross an equality.
     * @param diffs std::vector of Diff objects.
     *
    void diff_cleanupMerge(Diffs diffs) {
      diffs.push_back(Diff(EQUAL, ""));  // Add a dummy entry at the end.
      std::vectorIterator<Diff> pointer = diffs.listIterator();
      int count_delete = 0;
      int count_insert = 0;
      Range text_delete = "";
      Range text_insert = "";
      Diff thisDiff = pointer.next();
      Diff prevEqual = null;
      int commonlength;
      while (thisDiff != null) {
        switch (thisDiff.operation) {
        case INSERT:
          count_insert++;
          text_insert += thisDiff.text;
          prevEqual = null;
          break;
        case DELETE:
          count_delete++;
          text_delete += thisDiff.text;
          prevEqual = null;
          break;
        case EQUAL:
          if (count_delete + count_insert > 1) {
            bool both_types = count_delete != 0 && count_insert != 0;
            // Delete the offending records.
            pointer.previous();  // Reverse direction.
            while (count_delete-- > 0) {
              pointer.previous();
              pointer.remove();
            }
            while (count_insert-- > 0) {
              pointer.previous();
              pointer.remove();
            }
            if (both_types) {
              // Factor out any common prefixies.
              commonlength = diff_commonPrefix(text_insert, text_delete);
              if (commonlength != 0) {
                if (pointer.hasPrevious()) {
                  thisDiff = pointer.previous();
                  assert thisDiff.operation == EQUAL
                         : "Previous diff should have been an equality.";
                  thisDiff.text += text_insert.substring(0, commonlength);
                  pointer.next();
                } else {
                  pointer.push_back(Diff(EQUAL,
                      text_insert.substring(0, commonlength)));
                }
                text_insert = text_insert.substring(commonlength);
                text_delete = text_delete.substring(commonlength);
              }
              // Factor out any common suffixies.
              commonlength = diff_commonSuffix(text_insert, text_delete);
              if (commonlength != 0) {
                thisDiff = pointer.next();
                thisDiff.text = text_insert.substring(text_insert.size()
                    - commonlength) + thisDiff.text;
                text_insert = text_insert.substring(0, text_insert.size()
                    - commonlength);
                text_delete = text_delete.substring(0, text_delete.size()
                    - commonlength);
                pointer.previous();
              }
            }
            // Insert the merged records.
            if (text_delete.size() != 0) {
              pointer.push_back(Diff(DELETE, text_delete));
            }
            if (text_insert.size() != 0) {
              pointer.push_back(Diff(INSERT, text_insert));
            }
            // Step forward to the equality.
            thisDiff = pointer.hasNext() ? pointer.next() : null;
          } else if (prevEqual != null) {
            // Merge this equality with the previous one.
            prevEqual.text += thisDiff.text;
            pointer.remove();
            thisDiff = pointer.previous();
            pointer.next();  // Forward direction
          }
          count_insert = 0;
          count_delete = 0;
          text_delete = "";
          text_insert = "";
          prevEqual = thisDiff;
          break;
        }
        thisDiff = pointer.hasNext() ? pointer.next() : null;
      }
      if (diffs.getLast().text.size() == 0) {
        diffs.removeLast();  // Remove the dummy entry at the end.
      }

      /*
       * Second pass: look for single edits surrounded on both sides by
    equalities
       * which can be shifted sideways to eliminate an equality.
       * e.g: A<ins>BA</ins>C -> <ins>AB</ins>AC
       * /
      bool changes = false;
      // Create a new iterator at the start.
      // (As opposed to walking the current one back.)
      pointer = diffs.listIterator();
      Diff prevDiff = pointer.hasNext() ? pointer.next() : null;
      thisDiff = pointer.hasNext() ? pointer.next() : null;
      Diff nextDiff = pointer.hasNext() ? pointer.next() : null;
      // Intentionally ignore the first and last element (don't need checking).
      while (nextDiff != null) {
        if (prevDiff.operation == EQUAL &&
            nextDiff.operation == EQUAL) {
          // This is a single edit surrounded by equalities.
          if (thisDiff.text.endsWith(prevDiff.text)) {
            // Shift the edit over the previous equality.
            thisDiff.text = prevDiff.text
                + thisDiff.text.substring(0, thisDiff.text.size()
                                             - prevDiff.text.size());
            nextDiff.text = prevDiff.text + nextDiff.text;
            pointer.previous(); // Walk past nextDiff.
            pointer.previous(); // Walk past thisDiff.
            pointer.previous(); // Walk past prevDiff.
            pointer.remove(); // Delete prevDiff.
            pointer.next(); // Walk past thisDiff.
            thisDiff = pointer.next(); // Walk past nextDiff.
            nextDiff = pointer.hasNext() ? pointer.next() : null;
            changes = true;
          } else if (thisDiff.text.startsWith(nextDiff.text)) {
            // Shift the edit over the next equality.
            prevDiff.text += nextDiff.text;
            thisDiff.text = thisDiff.text.substring(nextDiff.text.size())
                + nextDiff.text;
            pointer.remove(); // Delete nextDiff.
            nextDiff = pointer.hasNext() ? pointer.next() : null;
            changes = true;
          }
        }
        prevDiff = thisDiff;
        thisDiff = nextDiff;
        nextDiff = pointer.hasNext() ? pointer.next() : null;
      }
      // If shifts were made, the diff needs reordering and another shift sweep.
      if (changes) {
        diff_cleanupMerge(diffs);
      }
    }
    */

    /**
     * Compute and return the source text (all equalities and deletions).
     * @param diffs std::vector of Diff objects.
     * @return Source text.
     */
    String diff_text1(Diffs diffs) {
        Range text{};
        for (Diff aDiff : diffs) {
            if (aDiff.operation != INSERT) {
                text.append(aDiff.text);
            }
        }
        return text;
    }

    /**
     * Compute and return the destination text (all equalities and insertions).
     * @param diffs std::vector of Diff objects.
     * @return Destination text.
     */
    String diff_text2(Diffs diffs) {
        Range text{};
        for (Diff aDiff : diffs) {
            if (aDiff.operation != DELETE) {
                text.append(aDiff.text);
            }
        }
        return text;
    }

    struct Stats {
        Size equal, inserted, deleted;
        Stats() : equal{0}, inserted{0}, deleted{0} {}
    };

    Stats stats() const {
        Stats ret;
        for (const auto &i : result) {
            switch (i.operation) {
                case EQUAL:  ret.equal += i.text.size(); break;
                case INSERT: ret.inserted += i.text.size(); break;
                case DELETE: ret.deleted += i.text.size(); break;
            }
        }
        return ret;
    }

};

#endif
