package forcomp

import scala.io.{Codec, Source}

object Anagrams extends AnagramsInterface :

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   * how often the character appears.
   * This list is sorted alphabetically w.r.t. to the character in each pair.
   * All characters in the occurrence list are lowercase.
   *
   * Any list of pairs of lowercase characters and their frequency which is not sorted
   * is **not** an occurrence list.
   *
   * Note: If the frequency of some character is zero, then that character should not be
   * in the list.
   */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   * It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = Dictionary.loadDictionary

  /** Converts the word into its character occurrence list.
   *
   * Note: the uppercase and lowercase version of the character are treated as the
   * same character, and are represented as a lowercase character in the occurrence list.
   *
   * Note: you must use `groupBy` to implement this method!
   */
  def wordOccurrences(w: Word): Occurrences =
    w.groupBy(c => c.toLower)
      .map { case (c, s) => (c, s.length) }
      .toList
      .sorted

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences =
    s.flatMap(wordOccurrences)
      .groupBy { case (c, i) => c }
      .map { case (c, s) => (c, s.map(_._2).sum) }
      .toList
      .sorted


  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   * the words that have that occurrence count.
   * This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   * For example, the word "eat" has the following character occurrence list:
   *
   * `List(('a', 1), ('e', 1), ('t', 1))`
   *
   * Incidentally, so do the words "ate" and "tea".
   *
   * This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   * List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] =
    dictionary.groupBy(wordOccurrences)
      .withDefaultValue(List.empty[Word])

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] =
    dictionaryByOccurrences(wordOccurrences(word))

  /** Returns the list of all subsets of the occurrence list.
   * This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   * is a subset of `List(('k', 1), ('o', 1))`.
   * It also include the empty subset `List()`.
   *
   * Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   * List(
   * List(),
   * List(('a', 1)),
   * List(('a', 2)),
   * List(('b', 1)),
   * List(('a', 1), ('b', 1)),
   * List(('a', 2), ('b', 1)),
   * List(('b', 2)),
   * List(('a', 1), ('b', 2)),
   * List(('a', 2), ('b', 2))
   * )
   *
   * Note that the order of the occurrence list subsets does not matter -- the subsets
   * in the example above could have been displayed in some other order.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] =

    def lengthCombinations(ch: Char, len: Int): Occurrences =
      (1 until len).toList.map(i => (ch, i))

    def check(occ: Occurrences): Boolean =
      occ.groupBy((ch, i) => ch).forall((k, l) => l.size == 1)

    val addByLength = occurrences.map(lengthCombinations)
    val fullMatrix = addByLength.flatten ::: occurrences
    val filteredMatrix = fullMatrix.toSet.subsets.toList
    val unsorted = filteredMatrix.map(_.toList).filter(check)
    unsorted.map(_.sorted)

  /** Subtracts occurrence list `y` from occurrence list `x`.
   *
   * The precondition is that the occurrence list `y` is a subset of
   * the occurrence list `x` -- any character appearing in `y` must
   * appear in `x`, and its frequency in `y` must be smaller or equal
   * than its frequency in `x`.
   *
   * Note: the resulting value is an occurrence - meaning it is sorted
   * and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences =

    def subChar(list: List[(Char, Int)]): List[(Char, Int)] =
      if list.length == 1 then
        list
      else
        val (c1, i1) = list.head
        val (c2, i2) = list.tail.head
        List(c1 -> (i1 - i2))

    (x ::: y).groupBy { case (ch, i) => ch }
      .values
      .toList
      .flatMap(subChar)
      .filter { case (ch, i) => i != 0 }
      .sorted

  /** Returns a list of all anagram sentences of the given sentence.
   *
   * An anagram of a sentence is formed by taking the occurrences of all the characters of
   * all the words in the sentence, and producing all possible combinations of words with those characters,
   * such that the words have to be from the dictionary.
   *
   * The number of words in the sentence and its anagrams does not have to correspond.
   * For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   * Also, two sentences with the same words but in a different order are considered two different anagrams.
   * For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   * `List("I", "love", "you")`.
   *
   * Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   * List(
   * List(en, as, my),
   * List(en, my, as),
   * List(man, yes),
   * List(men, say),
   * List(as, en, my),
   * List(as, my, en),
   * List(sane, my),
   * List(Sean, my),
   * List(my, en, as),
   * List(my, as, en),
   * List(my, sane),
   * List(my, Sean),
   * List(say, men),
   * List(yes, man)
   * )
   *
   * The different sentences do not have to be output in the order shown above - any order is fine as long as
   * all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   * Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   * so it has to be returned in this list.
   *
   * Note: There is only one anagram of an empty sentence.
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] =
    val occ = sentenceOccurrences(sentence)

    def loop(occ: Occurrences, branch: List[Occurrences], res: List[List[Occurrences]]): List[List[Occurrences]] =
      val comb = combinations(occ).filter(dictionaryByOccurrences.contains)
      if occ.isEmpty then
        branch.reverse :: res
      else if comb.isEmpty then
        List(Nil)
      else
        comb.map(o => loop(subtract(occ, o), o :: branch, res)).reduce(_ ::: _)

    def iter(listOcc: List[Occurrences], branch: Sentence, res: List[Sentence]): List[Sentence] =
      if listOcc.isEmpty then
        branch :: res
      else
        val words = dictionaryByOccurrences(listOcc.head)
        words.map(w => iter(listOcc.tail, w :: branch, res)).reduce(_ ::: _)

    if sentence.isEmpty then
      List(Nil)
    else
      val occCombList = loop(occ, Nil, List(Nil)).filterNot(_ == Nil)
      val sentList = occCombList.flatMap(listOcc => iter(listOcc, Nil, Nil))
      sentList

object Dictionary:
  def loadDictionary: List[String] =
    val wordstream = Option {
      getClass.getResourceAsStream(List("forcomp", "linuxwords.txt").mkString("/", "/", ""))
    } getOrElse {
      sys.error("Could not load word list, dictionary file not found")
    }
    try
      val s = Source.fromInputStream(wordstream)(Codec.UTF8)
      s.getLines().toList
    catch
      case e: Exception =>
        println("Could not load word list: " + e)
        throw e
    finally
      wordstream.close()

  import forcomp.Anagrams._

  @main def run() = sentenceAnagrams(List("my", "a"))