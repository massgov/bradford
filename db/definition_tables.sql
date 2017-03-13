CREATE OR REPLACE VIEW conversions AS

SELECT
      cast(maxes.hit_timestamp::TIMESTAMP WITHOUT TIME ZONE AT TIME ZONE 'America/New_York' AS DATE) as hit_date,
      concat(features.medium, '-', features.source) AS referrer,
      des_info.node_id,
      des_info.content_type                         AS child_type,
      des_info.title                                AS child_title,
      e.event_action                                AS action,
      e.event_category                              AS category,
      e.event_label                                 AS label,
      count(*)                                      AS conversions
    FROM (SELECT s.session_id,
           s.node_id,
          s.hit_timestamp,
          max(hit_timestamp) OVER (PARTITION BY session_id ORDER BY hit_timestamp DESC) as max
            FROM ga_session as s) AS maxes
      LEFT OUTER JOIN ga_node ON maxes.session_id = ga_node.session_id AND maxes.node_id = ga_node.node_id
      LEFT OUTER JOIN ga_events AS e ON e.page_path = ga_node.page_path AND e.session_id = maxes.session_id
      LEFT OUTER JOIN ga_session_features as features ON e.session_id = features.session_id
      LEFT OUTER JOIN drupal_nodes AS des_info ON ga_node.node_id = des_info.node_id
    WHERE maxes.max = maxes.hit_timestamp AND event_action IN ('FileDownload','Phone Number Link','QuickAction','Email Link')
    GROUP BY e.event_action,
      e.event_category,
      e.page_path,
      des_info.node_id,
      des_info.content_type,
      des_info.title,
      cast(maxes.hit_timestamp::TIMESTAMP WITHOUT TIME ZONE AT TIME ZONE 'America/New_York' AS DATE),
      features.medium,
      features.source,
      e.event_label


CREATE OR REPLACE VIEW sessions AS
SELECT s.node_id as node_id, 
       count(s.node_id) / cast(totals.total_views as FLOAT) as sessions,
       cast(s.hit_timestamp::TIMESTAMP WITHOUT TIME ZONE AT TIME ZONE 'America/New_York' AS DATE) as hit_date,
       feature.medium, 
       feature.device_category,
       feature.operating_system,
       feature.source, 
       feature.browser,
       node_info.content_type,
       node_info.title                                   
FROM ga_session as s                                                                                                             
INNER JOIN (SELECT session_id, count(*) as total_views FROM ga_session GROUP BY session_id) as totals ON totals.session_id = s.session_id
LEFT OUTER JOIN ga_session_features AS feature ON s.session_id = feature.session_id
LEFT OUTER JOIN drupal_nodes AS node_info ON node_info.node_id = s.node_id
GROUP BY s.node_id, 
        totals.total_views, node_info.content_type, node_info.title,
        feature.medium, feature.device_category, feature.operating_system, 
        feature.source, feature.browser, cast(s.hit_timestamp::TIMESTAMP WITHOUT TIME ZONE AT TIME ZONE 'America/New_York' AS DATE)

--select sum(sessions) from sessions == select count(DISTINCT session_id) from ga_session
