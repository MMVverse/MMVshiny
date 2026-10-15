# Keep the on-disk snapshot cache of the tests out of the user's home.
options(MMVshiny.snapshotCacheDir = file.path(tempdir(), "MMVshinySnapshotCache"))
