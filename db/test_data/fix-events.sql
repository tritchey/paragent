alter table events add column TYPE_ID integer;
alter table events add index (TYPE_ID);

update events set TYPE_ID = 0 where SUMMARY like "%offline%";
update events set TYPE_ID = 1 where SUMMARY like "%online%";

update events set TYPE_ID = 2 where SUMMARY like "%motherboard%added%";
update events set TYPE_ID = 3 where SUMMARY like "%motherboard%removed%";

update events set TYPE_ID = 2 where SUMMARY like "%sound device%added%";
update events set TYPE_ID = 3 where SUMMARY like "%sound device%removed%";

update events set TYPE_ID = 2 where SUMMARY like "%processor%added%";
update events set TYPE_ID = 3 where SUMMARY like "%processor%removed%";

update events set TYPE_ID = 2 where SUMMARY like "%card%added%";
update events set TYPE_ID = 3 where SUMMARY like "%card%removed%";

update events set TYPE_ID = 2 where SUMMARY like "%drive%added%";
update events set TYPE_ID = 3 where SUMMARY like "%drive%removed%";

update events set TYPE_ID = 2 where SUMMARY like "%memory%added%";
update events set TYPE_ID = 3 where SUMMARY like "%memory%removed%";

update events set TYPE_ID = 2 where SUMMARY like "%bios%added%";
update events set TYPE_ID = 3 where SUMMARY like "%bios%removed%";

update events set TYPE_ID = 2 where SUMMARY like "%printer%added%";
update events set TYPE_ID = 3 where SUMMARY like "%printer%removed%";

update events set TYPE_ID = 4 where SUMMARY like "%software installed%";
update events set TYPE_ID = 5 where SUMMARY like "%software removed%";
update events set TYPE_ID = 6 where SUMMARY like "%hotfix installed%";
update events set TYPE_ID = 7 where SUMMARY like "%hotfix uninstalled%";
update events set TYPE_ID = 8 where events.ID in (select events.ID from alert_event_link where events.ID = alert_event_link.EVENT_ID);
update events set TYPE_ID = 9 where SUMMARY like "%service%added%";
update events set TYPE_ID = 10 where SUMMARY like "%service%removed%";
update events set TYPE_ID = 11 where SUMMARY like "%user%added%";
update events set TYPE_ID = 12 where SUMMARY like "%user%removed%";
update events set TYPE_ID = 13 where SUMMARY like "%hardware error%found%";
update events set TYPE_ID = 14 where SUMMARY like "%hardware error%fixed%";