#BioScala


BioScala is a library of Scala code for efficient management and processing of biological sequence data, along
with some simple data structures and algorithms for sequence analysis. It was developed as a learning project,
and as a framework to support exploration of the intersection of pure functional programming and bioinformatics.
Despite the name, it is not a port of, and has no relationship to any of the existing Bio* projects. 

The library uses immutable objects (base class "Sequence") and purely functional data structures to represent
sequence data and meta-data and to manage sequence analysis transformation pipelines. BioScala sequences
support two means of accessing underlying sequence data: a "push" model employing stream-style iteratee based
enumeration (see for example [Scalaz Tutorial: Enumeration-based I/O With Iteratees](http://blog.higher-order.com/blog/2010/10/14/scalaz-tutorial-enumeration-based-io-with-iteratees/ "Scalaz Tutorial: Enumeration-based I/O With Iteratees") or [Iteratees](http://jsuereth.com/scala/2012/02/29/iteratees.html)),
and a "pull" model employing Java-style iterators.


##Sequence Sources

All sequence objects in BioScala are backed by a "source" object (base class "SequenceSource") which represents the
actual sequence data. The source in turn may be backed by a String, a FASTA file, or an in-memory packed bit
representation (SequenceSource subclasses "SequenceSourceString", "SequenceSourceFASTA", and "SequenceSourceCache"
respectively), with the latter being backed by a packed Int vector (SequenceCachePacked - only used for DNA sequences).
Wherever possible, sequence transformations result in new sequences which share the backing source of the original
sequence.

For example, the following code creates a sequence in memory, backed by a String, and transcribes it to an
RNA sequence which shares the backing source of the original String based sequence:

    val dnaSeq: DNASequence = DNASequence("Seq ID", "acgt")
    val rnaSeq: RNASequence = dnaSeq.transcribe

Only the first sequence (dnaSeq) allocates memory for the String sequence data. The transcribed (rnaSeq) variable
is backed by a source that is represented as a function-mapped transformation (class "SequenceSourceMappedLinear") on
the immutable source backing the original dnaSeq variable. If the original sequence had been backed by a FASTA file source,
no memory would be allocated for the sequence data in either variable, since the underlying source is an on-disk file
that can be streamed on demand:

    val seq = DNASequence("my.fasta")
    val rnaSeq: RNASequence = dnaSeq.transcribe

Any subsequent enumeration or iteration of the rnaSeq variable in the example above would cause the original data
from the FASTA file to be streamed in from the dnaSeq variable; transformed on the fly through the RNA mapping
variable; and subsequently delivered to the consuming iteratee or iterator.

##Reification

In some cases, a backing source which is represented by a FASTA file may need to be reified (materialized) into memory.
One such example is reversing a sequence:

    val seq = DNASequence("my.fasta")
    val revSeq = seq.reverseComplement

In this case, BioScala will create a reified version of the original sequence for the reverse sequence, and use a
reverse cache (which is simply a SequenceSourceCache that enumerates and/or iterates through the cache in reverse)
as the backing source for the revSeq, allowing both the the original and reversed sequence to share the single
underlying data source.

Iterator-style access can only be used with reified sequences (either cache or String based). Iteratee-style
enumeration can be used with any underlying source. Any attempt to use an iterator on a sequence that is not
reified will cause the it to be automatically reified.


##Iteratee Style Computations

In BioScala, Sequence analysis is preferentially done using enumerator/iteratee style computation overiterator-style,
which requires algorithms that have "online" streaming implementations (for example, calculating GC content).

A simple example of iteratee-style enumeration (selecting the first and last 5 characters in the sequence) is:

    val seq = DNASequence("id", "aacccgtaacgtg")
    val res = seq.enumerate {
      for {
        a <- Iteratees.take[Char](5)
        b <- Iteratees.takeRight[Char](5)
      } yield (a, b)
    }.result
    println(res.get)

For algorithms that require access to the entire sequence (such as the algorithms used for the DeBruijn graph builder
and the simple Suffix Trie builder), the sequence can be reified into memory in packed bit representation
(currently only for DNA) and a standard iterator can be used. An "online" algorithm for Suffix Tree building exists,
but it is not currently implemented in BioScala.

## Algorithms

The algorithms supported by BioScala are fairly naive implementations of the following:

- Pairwise Hamming Distance
- Find a consensus string
- Find a literal motif
- Find overlap pairs (based on a DeBruijn graph)
- Longest common substring (based on a Suffix Tree)
- Find a pattern from a simple pattern language (based on a Suffix Tree)
- Find most frequent kmers (based on a Suffix Tree)
- Transcription and translation
- GC content calculation

##FASTA Files

BioScala currently supports two styles of reading sequences from FASTA files. The first way uses a FASTA file as a
backing source for a single sequence:

    val seq = DNASequence("my.fasta")

This usage only accesses the first sequence in the FASTA file.

The second style uses a FASTAFileReader object and an iteratee to process all of the sequences in a FASTA file:

    val ffr = new FASTAFileReader("my.fasta")

The FASTAFile reader's enumerateResultList method takes an iteratee and applies it to each sequence in the file,
returning a list of results. A shorthand way to get the sequences directly into memory is the reifySequencesPacked
method, which uses its own iteratee to reify the sequences. It returns a List[(seq_ID, SequenceSourceCachePacked)],
which can in turn be mapped into a List[Sequences]:

    val ffr = new FASTAFileReader("my.fasta")
    val sList = ffr.reifySequencesPacked.result.map(l => l.map(c => (c._1, DNASequence(c._1, new SequenceSourceCache(c._2)).getSequenceString())))

##Generic Types

The gentypes package contains implementations of some general classes for things such as iteratees and state
transformations, which I developed for this project as a reinforcing exercise. Long term, these implementations
can and should be replaced with the implementations in a library such as scalz.


##More Examples

For additional BioScala examples, see the test suites or the file BioScalaTestDriver.scala, which contains a series
of various sequence transformation examples.


##Next Steps

- Port the remaining RNA sequence (ORF and validProtein methods, etc) implementations from the first iteration of BioScala.
- Implement a Suffix Array to replace the Suffix Trie.
- Improve ad extend the FASTA file parser and add support for other file formats.


