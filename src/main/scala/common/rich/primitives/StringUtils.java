/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

// Modified from org.apache.commons.lang3.StringUtils to avoid pulling the entire dependency.
package common.rich.primitives;

class StringUtils {
  private static final int INDEX_NOT_FOUND = -1;

  public static String replace(String text, String searchString, String replacement) {
    if (text.isEmpty() || searchString.isEmpty())
      return text;
    int start = 0;
    int end = text.indexOf(searchString, start);
    if (end == INDEX_NOT_FOUND)
      return text;
    int increase = 64 * Math.max(replacement.length() - searchString.length(), 0);
    StringBuilder $ = new StringBuilder(text.length() + increase);
    while (end != INDEX_NOT_FOUND) {
      $.append(text, start, end).append(replacement);
      start = end + searchString.length();
      end = text.indexOf(searchString, start);
    }
    $.append(text, start, text.length());
    return $.toString();
  }
}
