''''''
from timdb.timdbbase import TimDbBase
from contracts import contract

class Answers(TimDbBase):
    
    @contract
    def saveAnswer(self, user_ids : 'list(int)', task_id : 'str', content : 'str', points : 'str|None'):
        """Saves an answer to the database.
        
        :param user_ids: The id of the usergroup to which the answer belongs.
        :param task_id: The id of the task.
        :param content: The content of the answer.
        :param points: Points for the task.
        """
        
        cursor = self.db.cursor()
        cursor.execute('insert into Answer (task_id, content, points, answered_on) values (?,?,?,CURRENT_TIMESTAMP)', [task_id, content, points])
        answer_id = cursor.lastrowid
        assert answer_id is not None
        
        for user_id in user_ids:
            cursor.execute('insert into UserAnswer (user_id, answer_id) values (?,?)', [user_id, answer_id])
        
        self.db.commit()
        
    @contract
    def getAnswers(self, user_id : 'int', task_id : 'str') -> 'list(dict)':
        """Gets the answers of a user in a task, ordered descending by submission time.
        
        :param user_id: The id of the user.
        :param task_id: The id of the task.
        """
        
        cursor = self.db.cursor()
        cursor.execute("""select * from Answer where task_id = ?
                          and id in
                              (select answer_id from UserAnswer where user_id = ?)
                          order by answered_on desc""", [task_id, user_id])
        
        return self.resultAsDictionary(cursor)

    @contract
    def getAnswersForGroup(self, user_ids : 'list(int)', task_id : 'str') -> 'list(dict)':
        """Gets the answers of the users in a task, ordered descending by submission time.
           All users in the list `user_ids` must be associated with the answer.
        
        """
        
        cursor = self.db.cursor()
        sql = """select * from Answer where task_id = ?
                          %s
                          order by answered_on desc""" % (" ".join(["and id in (select answer_id from UserAnswer where user_id = %d)" % user_id for user_id in user_ids]))
        print(sql)
        cursor.execute(sql, [task_id])
        return self.resultAsDictionary(cursor)

