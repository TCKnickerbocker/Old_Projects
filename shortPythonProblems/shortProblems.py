import random


def complement(dna_str):
  '''
 Purpose: To take a dna strand as a string input and return a string of the complementary dna strand
 Input Parameter(s):
 dna_str: a string of lowercase a, t, g, and cs
 Return Value:
 the list my_str, which is the complement of dna_str
 '''

  dna_str = dna_str.lower()
  my_str = ""
  for i in range(len(dna_str)):
    if dna_str[i] == "a":
     my_str += "t"
    if dna_str[i] == "t":
     my_str += "a"
    if dna_str[i] == "g":
     my_str += "c"
    if dna_str[i] == "c":
     my_str += "g"
  return my_str


def mark(dna_str):
 '''
 Purpose: To take a dna strand as a string input and return a string that replaces all codons of "atg" with ">>>" and all codons of "tag", "taa", and "tga" with "<<<"
 Input Parameter(s):
 dna_str: a string of lowercase a, t, g, and cs the length of which is divisible by three
 Return Value:
 the list dna_str, converted into its complement
 '''

 for i in range(0, len(dna_str)-2, 3):
   if dna_str[i:i+3] == "atg":
     dna_str = dna_str[0:i] + ">>>" + dna_str[i+3:]
   if dna_str[i:i+3] in ["tag", "taa", "tga"]:
     dna_str = dna_str[0:i] + "<<<" + dna_str[i+3:]

 return dna_str


def spam_table(spam, not_spam):
  '''
 Purpose: To take in two seperate lists output a   dictionary with the amount of times each word occurs in list one minus the amount of times it occurs in list two
 Input Parameter(s):
 spam = a list of strings containing many words
 not_spam = a list of strings containing many words
 Return Value(s):
 mydict, a dict containing the number of times each word occurs in the spam list minus the number of times each word occurs in the not_spam list
 '''

  spamlist = []
  for i in spam:
   spamlist+= i.split()
  for i in range(len(spamlist)):
    spamlist[i] = spamlist[i].lower()
  mydict = {}

  for i in range(len(spamlist)):
   neo_word = spamlist[i]
   mydict[neo_word] = 0

  for i in range(len(spamlist)):
   new_word = spamlist[i]
   mydict[new_word] += 1


  notspamlist = []
  for i in not_spam:
   notspamlist+= i.split()
  for i in range(len(notspamlist)):
    notspamlist[i] = notspamlist[i].lower()

  for i in range(len(notspamlist)):
   t_word = notspamlist[i]
   if t_word not in mydict:
     mydict[t_word] = 0

  for i in range(len(notspamlist)):
   new_word = notspamlist[i]
   mydict[new_word] -= 1

  return mydict


def spam_check(email, table):
  '''
 Purpose: To take in an email and a table of values for certain words, and compute the total "spam score" for that email. If the spamscore is positive, print "Spam", and if it isn't, print "Not Spam". Then return the spam score value
 Input Parameter(s):
 email: a string containing multiple words
 table: a dictionary containing the "spam score" of certain words
 Return Value:
 spam_score, the net "score" of the string
  '''

  spam_score = 0
  Dlist = []

  Dlist = email.split()

  for i in range(len(Dlist)):
    Dlist[i] = Dlist[i].lower()

  for i in range(len(Dlist)):
   if Dlist[i] in table:
     spam_score += table[Dlist[i]]

  if spam_score <= 0:
   print("Not Spam")
  elif spam_score > 0:
   print("Spam")
  return spam_score


def weighted_select(input_dictionary):
  '''
  Purpose: make a function taking a ditionary of words and their corresponding counts. Function should randomly choose a word, using its count as a weight in the selection.
  Input Paramater(s):
  input_dictionary: a dicitonary containing words and their corresponding values
  Return Value(s):
  results.count(for all words): shows the amount of times the word came up
  '''

  rando_list = []
  for i in input_dictionary:
    key = i
    val = input_dictionary[i]
    for i in range(val):
     rando_list.append(key)

  bahaha = rando_list[random.randint(0, len(rando_list)-1)]
  return bahaha


def bigram_count(string):
  '''
  Purpose: make a function taking a string and returning a dictionary containing all consecutive words within the string, and each time that pair of words appears together as the value
  Input Paramater(s):
  string: a user-entered string that will be counted by our dictionary
  Return Value(s):
  bigram_dict: a dictionary containing each word as a key and a smaller dictionary containing the number of times a specific word follows that key word in the sentence as a value
  '''

  a = string.split()
  bigram_dict = {}
  h = []

  for i in range(len(a[:])-1):
    current = a[i]
    nextWord = a[i+1]
    minidict = {}

    if current not in bigram_dict:
      bigram_dict[current] = {nextWord:1}

    else:
      minidict = bigram_dict[current]
      if nextWord in minidict:
        minidict[nextWord] += 1
      else:
        minidict[nextWord] = 1

  return bigram_dict


def random_sentence(bigram_example, starting_word, integer):
  '''
  Purpose: create a function that makes a randomly generated sentence of integer length, using starting_word as the first word and bigram_example as reference for word weights. each word should use its bigram_example weighing to randomly determine which word will follow it in the sentence
  Input Paramater(s):
  bigram_example: the inputted bigram_dict used to determine word weighted_select
  starting_word: the word that will be used as the first word in our sentence, as well as the default word if the selected word doesn't appear in the bigram_example dictionary
  integer: the length of our sentence (2 would mean 2 words)
  Return Value(s):
  sentence: a semi-randomly generated sentenceof integer length, beggining with the starting_word and made by utilizing our bigram_dict
  '''

  sentence = ""
  lastword = ""
  for i in range(integer):
    word = ""
    h = []

    if (i == 0) or (lastword not in bigram_example):
      word = starting_word

    else:
      for x in bigram_example[lastword]:
        for j in range(bigram_example[lastword][x]):
          h.append(x)
      indexno = random.randint(0, len(h)-1)

      word = h[indexno]

    sentence = sentence + word + " "
    lastword = word

  return sentence


def rand_sent_file(filename, length):
  '''
  Purpose: make a function taking in the name of a file and the length of the desired random sentence and make a sentence of that length using weights analyzed from the rest of the text using the functions made earlier in this homework, starting at a random word within the file
  Input Paramater(s):
  filename: the name of the file we want to analyzed
  length: the length of the sentence we want to make
  Return Value(s):
  randSent = a random sentence made using the weights from the file
  '''

  randsent = ""

  f = open(filename, "r")
  contents = f.read()
  contents = contents.replace("\n", " ")

  bDict = bigram_count(contents)

  newContents = contents.split(" ")

  indexno = random.randint(0, len(newContents)-1)
  starting_word = newContents[indexno]
  randsent = random_sentence(bDict, starting_word, length)

  f.close()

  return randsent
