from hanzo.storage.files import FileStore
from hanzo.storage.google_cloud import GoogleCloudFileStore
from hanzo.storage.local import LocalFileStore
from hanzo.storage.memory import InMemoryFileStore
from hanzo.storage.s3 import S3FileStore


def get_file_store(file_store: str, file_store_path: str | None = None) -> FileStore:
    if file_store == 'local':
        if file_store_path is None:
            raise ValueError('file_store_path is required for local file store')
        return LocalFileStore(file_store_path)
    elif file_store == 's3':
        return S3FileStore(file_store_path)
    elif file_store == 'google_cloud':
        return GoogleCloudFileStore(file_store_path)
    return InMemoryFileStore()
