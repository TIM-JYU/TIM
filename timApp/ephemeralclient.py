'''
An Ephemeral client that can communicate with Ephemeral server.
'''
import urllib.request
from contracts import contract

class EphemeralClient(object):
    
    @contract
    def __init__(self, server_path: 'str'):
        self.server_path = server_path

    @contract
    def addBlock(self, document_id : 'int', next_block_id : 'int', content : 'str') -> 'bool':
        """Adds a block to a document.
        
        :param document_id: The id of the document.
        :param next_block_id: The id of the following block.
        :param content: The content of the block.
        """
        req = urllib.request.Request(url=self.server_path + '/add/{}/{}'.format(document_id, next_block_id), data=bytes(new_content, encoding='utf-8'), method='PUT')
        response = urllib.request.urlopen(req)
        responseStr = str(response.read(), encoding='utf-8')
        
        # TODO: Handle errors.
        
        return True

    def deleteBlock(self, document_id, block_id):
        """Deletes a block from a document.
        
        :param document_id: The id of the document.
        :param block_id: The id of the block to be deleted.
        :returns: True if deletion was successful, false otherwise.
        """
        req = urllib.request.Request(url=self.server_path + '/delete/{}/{}'.format(document_id, next_block_id), method='PUT')
        response = urllib.request.urlopen(req)
        responseStr = str(response.read(), encoding='utf-8')
        
        # TODO: Handle errors.
        
        return True

    @contract
    def diff(self, first_document_id : 'int', second_document_id : 'int') -> 'str':
        req = urllib.request.Request(url=self.server_path + '/diff/{}/{}'.format(first_document_id, second_document_id), method='GET')
        response = urllib.request.urlopen(req)
        return str(response.read(), encoding='utf-8')
    
    @contract
    def diff3(self, first_document_id : 'int', second_document_id : 'int', third_document_id : 'int') -> 'str':
        req = urllib.request.Request(url=self.server_path + '/diff/{}/{}'.format(first_document, second_document), method='GET')
        response = urllib.request.urlopen(req)
        return str(response.read(), encoding='utf-8')
    
    @contract
    def getBlock(self, document_id : 'int', block_id : 'int') -> 'str':
        req = urllib.request.Request(url=self.server_path + '/{}/{}'.format(document_id, block_id), method='GET')
        response = urllib.request.urlopen(req)
        return str(response.read(), encoding='utf-8')
    
    @contract
    def getDocumentAsBlocks(self, document_id: 'int') -> 'str':
        # TODO: Ephemeral doesn't support this yet.
        req = urllib.request.Request(url=self.server_path + '/{}'.format(document_id), method='GET')
        return ''
    
    @contract
    def getDocumentFullText(self, document_id : 'int') -> 'str':
        """Gets the full text of a document.
        
        :param document_id: The id of the document whose text will be fetched.
        :returns: The text of the document.
        """
        
        req = urllib.request.Request(url=self.server_path + '/{}'.format(document_id), method='GET')
        response = urllib.request.urlopen(req)
        return str(response.read(), encoding='utf-8')
    
    @contract
    def loadDocument(self, document_id : 'int', content : 'bytes') -> 'bool':
        """Loads a new document to Ephemeral.
        
        :param document_id: The id of the document.
        :param content: The content of the document.
        :returns: True if the document was successfully loaded, false otherwise.
        """
        req = urllib.request.Request(url=self.server_path + '/load/{}'.format(document_id), data=content, method='POST')
        response = urllib.request.urlopen(req)
        
        # TODO: Handle errors.
        
        return True
    
    @contract
    def modifyBlock(self, document_id : 'int', block_id : 'int', new_content: 'str') -> 'bool':
        req = urllib.request.Request(url=self.server_path + '/{}/{}'.format(document_id, block_id), data=bytes(new_content, encoding='utf-8'), method='PUT')
        response = urllib.request.urlopen(req)
        responseStr = str(response.read(), encoding='utf-8')
        
        # TODO: Handle errors.
        
        return True
