Hello All,

I am writing a program that will define a bunch of tasks. Each task
will have to implement certain methods.


data UpdateAcctsTask = UpdateAccts

data EmailConfig = EmaiConfig {someattrs::String}
data SendEmailTask = SendEmailsTask EmailConfig

data GeneralWriterTask a = GeneralWriterTask a

Each of these tasks implement a class, Taskable. The return
values are simplified for this example.

class Taskable a where
  process :: a -> Bool
  can_run :: a -> Bool


This works fine. I can expand on these tasks and execute them.

Now, I wanted to be able to defined dependencies of these (Tasksables). I decided
I could create a data type for this dependency and may be also construct a FreeMonad
around this structure for further processing using a graph of Tasks.

Therefore, first thing I did was, define a Task, which generalizes over all
the above defined (and future Taskables)

data Task a where
  Task :: (Taskable a) => a -> Task a


instance Functor Task where
  fmap:: (Taskable a, Taskable b) -> (a -> b) -> Task a  -> Task b
  fmap f (Task a) = Task $ f a


But, I realized that I cannot define an fmap over a type constraint.

My questions are:

1. Is there any way to do this. I see there is an answer of SO. I wanted
   to make sure if there were any improvements to this since that answer'
   was posted.

2. Secondly, I would like to know why this is not possible. Is it a
