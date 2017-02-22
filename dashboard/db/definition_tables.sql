SELECT
        e.event_action as action,
        e.event_category as category,
        des_info.node_id as page_id,
        des_info.content_type as child_type,
        des_info.title as child_title,
        concat(features.medium,'-',features.source) as referer,
        cast(s.hit_timestamp + INTERVAL '1 hour' * s.offset_h as DATE) AS date,
        count(DISTINCT e.session_id) AS conversions
 INTO conversions
 FROM ga_events AS e
 LEFT OUTER JOIN ga_session as s ON e.session_id = s.session_id AND e.page_path = s.page_path
 LEFT OUTER JOIN ga_session_features as features ON e.session_id = features.session_id
 LEFT OUTER JOIN drupal_nodes AS des_info ON replace(des_info.node_path, 'https://pilot.mass.gov', '') = e.page_path
GROUP BY
  e.event_action,
  e.event_category,
  e.page_path,
  des_info.node_id,
  des_info.content_type,
  des_info.title,
  cast(s.hit_timestamp + INTERVAL '1 hour' * s.offset_h as DATE),
  features.medium,
  features.source)

-- SELECT sum(conversions) FROM conversions == SELECT count(*) FROM ga_events, there might be some differences for dates

SELECT s.session_id, 
       s.node_id, 
       count(s.node_id) / cast(totals.total_views as FLOAT) as sessions,
       min(cast(s.hit_timestamp + INTERVAL '1 hour' * s.offset_h as DATE)) as date,
       feature.medium, 
       feature.device_category,
       feature.operating_system,
       feature.source, 
       feature.browser,
       node_info.content_type,
       node_info.title
INTO sessions                                    
FROM ga_session as s                                                                                                             
INNER JOIN (SELECT session_id, count(*) as total_views FROM ga_session GROUP BY session_id) as totals ON totals.session_id = s.session_id
LEFT OUTER JOIN ga_session_features AS feature ON s.session_id = feature.session_id
LEFT OUTER JOIN drupal_nodes AS node_info ON node_info.node_id = s.node_id
GROUP BY s.session_id, s.node_id, 
        totals.total_views, node_info.content_type, node_info.title,
        feature.medium, feature.device_category, feature.operating_system, feature.source, feature.browser

--select sum(sessions) from sessions == select count(DISTINCT session_id) from ga_session
