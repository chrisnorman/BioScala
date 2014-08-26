#BioScala


BioScala is a library of Scala code for efficient processing of biological sequence data, and is the second iteration of a library I developed as I explored pure functional programming, Scala, and bioinformatics algorithms. It is not a port of, and has no relationship to, any of the existing Bio* projects. 

The library uses immutable objects (base class "Sequence") to represent sequence meta-data, and purely functional, immutable data structures and functional, iteratee-style computations (see for example [Scalaz Tutorial: Enumeration-based I/O With Iteratees](http://blog.higher-order.com/blog/2010/10/14/scalaz-tutorial-enumeration-based-io-with-iteratees/ "Scalaz Tutorial: Enumeration-based I/O With Iteratees") or [Iteratees](http://jsuereth.com/scala/2012/02/29/iteratees.html)) to efficiently manage sequence analysis transformation pipelines.


##Sequence Sources

All sequence objects in BioScala are backed by a "source" object (base class "SequenceSource") which represents the actual sequence data. The source in turn may be backed by a String, a FASTA file, or an in-memory packed bit representation (SequenceSource subclasses "SequenceSourceString", "SequenceSourceFASTA", and "SequenceSourceCache" respectively), with the latter being backed by an Int vector (SequenceCachePacked - only used for DNA and RNA sequences). Wherever possible, sequence transformations result in new sequences which share the backing source of the original sequence. For example, the following code creates a sequence in memory, backed by a String, and transcribes it to an RNA sequence, which shares the backing source of the original (String based) sequence:

    val dnaSeq: DNASequence = DNASequence("Seq ID", "acgt")
    val rnaSeq: RNASequence = dnaSeq.transcribe

Although there are two sequence variables above, only the first sequence actually allocates memory for the sequence data. The transcribed(rnaSeq) variable is backed by a source that is represented as a mapped transformation (class "SequnceSourceMappedLinear") of the original dnaSeq sequence. If the original sequence had been backed by a FASTA file source, the transcription transformation would result in two sequences with no memory allocated for the sequence data, since the underlying source is an on-disk file:

    val seq = DNASequence("my.fasta")
    val rnaSeq: RNASequence = dnaSeq.transcribe

Sequence sources support two means of accessing the underlying sequence data: "reactive stream style" iteratee based enumeration (push), and iteration style (pull).


##Reification

In some cases, a backing source which is represented by a FASTA file may need to be reified into memory. One such example is reversing a sequence:

    val seq = DNASequence("my.fasta")
    val revSeq = seq.reverseComplement

In this case, BioScala will create a reified version of the original sequence for the reverse sequence, and use a reverse zache (which is simply a SequenceSourceCache that enumerates and/or iterates through the cache in reverse) as the backing source for the revSeq.

Only reified sequences (either cached or String based) maybe used with iteration-style access (iteratee-style enumeration may be used with any sequence no matter what the underlying source). Any attempt to use an iterator on a sequence that is not reified will automatically force it to be reified.


##Iteratee Style Computations

Sequence analysis is preferentially done using the former (enumerator/iterate) based computation for any algorithm that allows "online" streaming (for example, calculating GC content). For algorithms that require access to the entire sequence (for example, a DeBruijn graph builder or simple suffix trie builder), the sequence can be reified into memory in packed bit representation and a standard iterator can be used.

A simple example of iteratee-style enumeration (selecting the first and last 5 characters in the sequence) is:

    val seq = DNASequence("id", "aacccgtaacgtg")
    val res = seq.enumerate {
      for {
        a <- Iteratees.take[Char](5)
        b <- Iteratees.takeRight[Char](5)
      } yield (a, b)
    }.result
    println(res.get)


##FASTA Files

BioScala currently supports two styles of reading sequences from FASTA files. The first way uses a FASTA file as a backing source for a single sequence:

    val seq = DNASequence("my.fasta")

This usage only accesses the first sequence in the FASTA file.

The second style uses a FASTAFileReader object and an iteratee to process all of the sequences in a FASTA file:

    val ffr = new FASTAFileReader("my.fasta")

The FASTAFile reader's enumerateResultList method takes an iteratee and applies it to each sequence in the file, returning a list of results. A shorthand way to get the sequences directly into memory is the reifySequencesPacked method, which uses its own iteratee to reify the sequences. It returns a List[(seq_ID, SequenceSourceCachePacked)], which can in turn be mapped into a List[Sequences]:

    val ffr = new FASTAFileReader("my.fasta")
    val sList = ffr.reifySequencesPacked.result.map(l => l.map(c => (c._1, DNASequence(c._1, new SequenceSourceCache(c._2)).getSequenceString())))

##Generic Types

The gentypes package contains implementations of some general classes for things such as iteratees and state transformations, which I developed for this project as a reinforcing exercise. Long term, these implementations can and should be replaced with the implementations in a library such as scalz.


##More Examples

For additional BioScala examples, see the test suites or the file BioScalaTestDriver.scala, which contains a series of various sequence transformation examples.


##Next Steps

Port the DeBruijn graph and Suffix Trie implementations from the first iteration of BioScala.

