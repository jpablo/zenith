import Z

import Lean.Data.HashMap

open Lean (HashMap)


namespace ServicePattern


  /- case class Doc -/
  structure Doc where
    title       : String
    description : String
    language    : String
    format      : String
    content     : ByteArray


  /- trait DocRepo -/
  structure DocRepo where
    get (id: String)           : ZTask Doc
    save (doc: Doc)            : ZTask String
    delete (id: String)        : ZTask Unit
    findByTitle (title: String): ZTask (List Doc)


  /- case class -/
  structure Metadata where
    title         : String
    description   : String
    language      : String
    format        : String

  /- trait -/
  structure MetadataRepo where
    get (id: String)                : ZTask Metadata
    put (id: String) (md: Metadata) : ZTask Unit
    delete (id: String)             : ZTask Unit
    findByTitle (title: String)     : ZTask (HashMap String Metadata)


  structure BlobStorage where
    get (id: String)        : ZTask ByteArray
    put (content: ByteArray): ZTask String
    delete (id: String)     : ZTask Unit




  /- class DocRepoImpl(metadataRepo, blobStorage) implements DocRepo -/
  namespace DocRepoImpl

    def create (metadataRepo: MetadataRepo) (blobStorage : BlobStorage) : DocRepo where

      -- override get
      get id := do
        let md <- metadataRepo.get id
        let content  <- blobStorage.get id
        return Doc.mk md.title md.description md.language md.format content

      save doc := do
        let id <- blobStorage.put doc.content
        metadataRepo.put id
          (Metadata.mk doc.title doc.description doc.language doc.format)
        return id

      delete id := do
        blobStorage.delete id
        metadataRepo.delete id

      findByTitle title := do
        let byTitle <- metadataRepo.findByTitle title
        let documents <- byTitle.toList.mapM fun (id, md) => do
          let content <- blobStorage.get id
          return Doc.mk md.title md.description md.language md.format content
        return documents


  end DocRepoImpl

end ServicePattern
