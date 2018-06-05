import random

class FastText(object):
    """ Class that implements the FastText algorithm for training word embeddings """

    def __init__(self):
        """ Class constructor, initialize variables"""
        # size of word embedding
        self.vector_size = 100
        # learningrate for optimization
        self.learning_rate = 0.025
        # number of iterations for optimization
        self.num_iterations = 1
        # seed used for PRNGs
        self.seed = random.randint
        # minimum word frequenc for filtering out un-frequent words
        self.min_count = 5
        # max length of a given sentence (used to compute contexts of words)
        self.max_sentence_length = 1000
        # max space between words in skipgram
        self.window = 5
        # number of training words
        self.train_words_count = 0
        # current vocabulary size
        self.vocab_size
        # word vocabulary
        self.vocab = []
        # min length of char n-gram
        self.minn = 3
        # max length of char n-gram
        self.maxn = 6
        # Boundary symbols to distinguish n-grams that are suffixes and prefixes from in-word ngrams
        self.EOS = "</s>"
        self.BOW = "<"
        self.EOW = ">"

    def learn_vocab(self, sentences):
        pass

    def compute_subwords(self, word, vocabsize, word_index):
        pass

    def softmax(self):
        pass

    def train(self):
        pass

