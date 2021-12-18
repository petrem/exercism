def add_me_to_the_queue(express_queue, normal_queue, ticket_type, person_name):
    """Add `person_name` to either `normal_queue` or `express_queue`.

    Choice is made depening on `ticket_type`.

    :param express_queue: list - names in the Fast-track queue.
    :param normal_queue:  list - names in the normal queue.
    :param ticket_type:  int - type of ticket. 1 = express, 0 = normal.
    :param person_name: str - name of person to add to a queue.
    :return: list - the (updated) queue the name was added to.
    """
    # The 2020s called, they want their match-case-lessness back...

    # match ticket_type:
    #     case 0:
    #         queue = normal_queue
    #     case 1:
    #         queue = express_queue
    #     case int(x):
    #         raise ValueError(f"ticket_type {x} is not one of 0 or 1.")
    #     case _:
    #         raise TypeError("ticket_type must be int")

    if ticket_type == 0:
        queue = normal_queue
    elif ticket_type == 1:
        queue = express_queue
    elif isinstance(ticket_type, int):
        raise ValueError(f"ticket_type {ticket_type} is not one of 0 or 1.")
    else:
        raise TypeError("ticket_type must be int")

    if person_name not in queue:
        queue.append(person_name)
    return queue


def find_my_friend(queue, friend_name):
    """Find `friend_name` in `queue`.

    :param queue: list - names in the queue.
    :param friend_name: str - name of friend to find.
    :return: int - index at which the friends name was found.
    """
    return queue.index(friend_name)


def add_me_with_my_friends(queue, index, person_name):
    """Insert `person_name` at `index`.

    :param queue: list - names in the queue.
    :param index: int - the index at which to add the new name.
    :param person_name: str - the name to add.
    :return: list - queue updated with new name.
    """
    queue.insert(index, person_name)
    return queue


def remove_the_mean_person(queue, person_name):
    """Remove first occurence of `person_name` from `queue`.

    :param queue: list - names in the queue.
    :param person_name: str - name of mean person.
    :return:  list - queue update with the mean persons name removed.
    """
    queue.remove(person_name)
    return queue


def how_many_namefellows(queue, person_name):
    """Count persons in `queue` with same `person_name`.

    :param queue: list - names in the queue.
    :param person_name: str - name you wish to count or track.
    :return:  int - the number of times the name appears in the queue.
    """
    return queue.count(person_name)


def remove_the_last_person(queue):
    """Remove last person from `queue`.

    :param queue: list - names in the queue.
    :return: str - name that has been removed from the end of the queue.
    """
    return queue.pop()


def sorted_names(queue):
    """Sort `queue`.

    :param queue: list - names in the queue.
    :return: list - copy of the queue in alphabetical order.
    """
    return sorted(queue)
