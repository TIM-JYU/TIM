'''
An Ephemeral client that can communicate with Ephemeral server.
'''
import urllib.request
import requests
import json
from contracts import contract, new_contract

new_contract('bytes', bytes)

class EphemeralException(Exception):
    pass

class NotInCacheException(Exception):
    pass

class EphemeralClient(object):
    
    @contract
    def __init__(self, server_path : 'str'):
        """Initializes EphemeralClient with the specified server path."""
        self.server_path = server_path

    @contract
    def addBlock(self, document_id : 'int', next_block_id : 'int', content : 'str') -> 'bool':
        """Adds a block to a document.
        
        :param document_id: The id of the document.
        :param next_block_id: The id of the following block.
        :param content: The content of the block.
        """
        req = urllib.request.Request(url=self.server_path + '/add/{}/{}'.format(document_id, next_block_id), data=bytes(content, encoding='utf-8'), method='PUT')
        response = urllib.request.urlopen(req)
        responseStr = str(response.read(), encoding='utf-8')
        
        # TODO: Handle errors.
        
        return True

    @contract
    def deleteBlock(self, document_id : 'int', block_id : 'int') -> 'bool':
        """Deletes a block from a document.
        
        :param document_id: The id of the document.
        :param block_id: The id of the block to be deleted.
        :returns: True if deletion was successful, false otherwise.
        """
        req = urllib.request.Request(url=self.server_path + '/delete/{}/{}'.format(document_id, block_id), method='PUT')
        response = urllib.request.urlopen(req)
        responseStr = str(response.read(), encoding='utf-8')
        
        # TODO: Handle errors.
        
        return True

    @contract
    def diff(self, first_document_id : 'int', second_document_id : 'int') -> 'str':
        """Performs a diff between two documents.
        
        :param first_document_id: The id of the first document.
        :param second_document_id: The id of the second document.
        :returns: (TODO)
        """
        req = urllib.request.Request(url=self.server_path + '/diff/{}/{}'.format(first_document_id, second_document_id), method='GET')
        response = urllib.request.urlopen(req)
        return str(response.read(), encoding='utf-8')
    
    @contract
    def diff3(self, first_document_id : 'int', second_document_id : 'int', third_document_id : 'int') -> 'str':
        """Performs a diff among three documents.
        
        :param first_document_id: The id of the first document.
        :param second_document_id: The id of the second document.
        :param third_document_id: The id of the third document.
        :returns: (TODO)
        """
        req = urllib.request.Request(url=self.server_path + '/diff/{}/{}'.format(first_document_id, second_document_id), method='GET')
        response = urllib.request.urlopen(req)
        return str(response.read(), encoding='utf-8')
    
    @contract
    def getBlock(self, document_id : 'int', block_id : 'int') -> 'str':
        """Gets an individual block from a document.
        
        :param document_id: The id of the document.
        :param block_id: The id of the block.
        :returns: The content of the block.
        """
        req = urllib.request.Request(url=self.server_path + '/{}/{}'.format(document_id, block_id), method='GET')
        response = urllib.request.urlopen(req)
        responseStr = str(response.read(), encoding='utf-8')
        if responseStr == '{"Error":"No block found"}':
            raise EphemeralException('No block found with document id %d and index %d' % (document_id, block_id))
        return responseStr
    
    @contract
    def getDocumentAsHtmlBlocks(self, document_id: 'int') -> 'list(str)':
        """Gets the document as a list of HTML blocks.
        
        :param document_id: The id of the document.
        :returns: The document as a list of HTML blocks.
        """
        req = urllib.request.Request(url=self.server_path + '/json-html/{}'.format(document_id), method='GET')
        response = urllib.request.urlopen(req)
        responseStr = str(response.read(), encoding='utf-8')
        if responseStr == '{"Error":"No doc found"}':
            raise NotInCacheException('No document found with id %d' % document_id)
        return json.loads(responseStr)
    
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
        """Modifies the specified block in the given document.
        
        :param document_id: The id of the document.
        :param block_id: The id of the block to be modified.
        :param new_content: The new content of the block.
        """
        #req = urllib.request.Request(url=self.server_path + '/{}/{}'.format(document_id, block_id), data=bytes(new_content, encoding='utf-8'), method='PUT')
        #response = urllib.request.urlopen(req)
        #responseStr = str(response.read(), encoding='utf-8')
        
        # requests library is much simpler to use:
        r = requests.put(url=self.server_path + '/{}/{}'.format(document_id, block_id), data=bytes(new_content, encoding='utf-8'))
        str = r.text
        # TODO: Handle errors.
        
        return True
