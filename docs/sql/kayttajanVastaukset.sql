select u.name, a.task_id, a.content, a.answered_on   
from answer as a, userAnswer as ua, user as u 
where a.task_id like "113073%" and ua.answer_id = a.id and u.id = ua.user_id 
order by ua.user_id;

select u.name, a.task_id, a.content, MIN(a.answered_on) 
from answer as a, userAnswer as ua, user as u 
where a.task_id like "113073%" and ua.answer_id = a.id and u.id = ua.user_id 
group by a.task_id, u.id
order by u.name 


Koevastaukset 24.4.2015:

select u.id,u.real_name,u.name, a.task_id, a.content, MIN(a.answered_on) 
from answer as a, userAnswer as ua, user as u 
where a.task_id like "113154.t1" and ua.answer_id = a.id and u.id = ua.user_id 
group by a.task_id, u.id
order by u.real_name 
