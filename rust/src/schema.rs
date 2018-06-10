use models::QuestionactivitykindMapping;
use models::ReadparagraphtypeMapping;

table! {
    accesstype (id) {
        id -> Int4,
        name -> Text,
    }
}

table! {
    alembic_version (version_num) {
        version_num -> Varchar,
    }
}

table! {
    annotation (id) {
        id -> Int4,
        velp_version_id -> Int4,
        icon_id -> Nullable<Int4>,
        annotator_id -> Int4,
        points -> Nullable<Float8>,
        creation_time -> Timestamptz,
        valid_from -> Nullable<Timestamptz>,
        valid_until -> Nullable<Timestamptz>,
        visible_to -> Nullable<Int4>,
        document_id -> Nullable<Int4>,
        answer_id -> Nullable<Int4>,
        paragraph_id_start -> Nullable<Text>,
        paragraph_id_end -> Nullable<Text>,
        offset_start -> Nullable<Int4>,
        node_start -> Nullable<Int4>,
        depth_start -> Nullable<Int4>,
        offset_end -> Nullable<Int4>,
        node_end -> Nullable<Int4>,
        depth_end -> Nullable<Int4>,
        hash_start -> Nullable<Text>,
        hash_end -> Nullable<Text>,
        color -> Nullable<Text>,
        element_path_start -> Nullable<Text>,
        element_path_end -> Nullable<Text>,
    }
}

table! {
    annotationcomment (id) {
        id -> Int4,
        annotation_id -> Int4,
        comment_time -> Timestamptz,
        commenter_id -> Int4,
        content -> Nullable<Text>,
    }
}

table! {
    answer (id) {
        id -> Int4,
        task_id -> Text,
        content -> Text,
        points -> Nullable<Float8>,
        answered_on -> Timestamptz,
        valid -> Bool,
        last_points_modifier -> Nullable<Int4>,
    }
}

table! {
    answertag (id) {
        id -> Int4,
        answer_id -> Int4,
        tag -> Text,
    }
}

table! {
    answerupload (upload_block_id) {
        upload_block_id -> Int4,
        answer_id -> Nullable<Int4>,
    }
}

table! {
    askedjson (asked_json_id) {
        asked_json_id -> Int4,
        json -> Text,
        hash -> Text,
    }
}

table! {
    askedquestion (asked_id) {
        asked_id -> Int4,
        lecture_id -> Int4,
        doc_id -> Nullable<Int4>,
        par_id -> Nullable<Text>,
        asked_time -> Timestamptz,
        points -> Nullable<Text>,
        asked_json_id -> Int4,
        expl -> Nullable<Text>,
    }
}

table! {
    block (id) {
        id -> Int4,
        latest_revision_id -> Nullable<Int4>,
        type_id -> Int4,
        description -> Nullable<Text>,
        created -> Timestamptz,
        modified -> Nullable<Timestamptz>,
    }
}

table! {
    blockaccess (block_id, usergroup_id, type_) {
        block_id -> Int4,
        usergroup_id -> Int4,
        #[sql_name = "type"]
        type_ -> Int4,
        accessible_from -> Nullable<Timestamptz>,
        accessible_to -> Nullable<Timestamptz>,
        duration -> Nullable<Interval>,
        duration_from -> Nullable<Timestamptz>,
        duration_to -> Nullable<Timestamptz>,
    }
}

table! {
    docentry (name) {
        name -> Text,
        id -> Int4,
        public -> Bool,
    }
}

table! {
    docgamified (gamification_doc_id, doc_id) {
        gamification_doc_id -> Int4,
        doc_id -> Int4,
        doc_type_id -> Nullable<Int4>,
    }
}

table! {
    documentgamificationpoint (doc_id, point_type_id) {
        doc_id -> Int4,
        point_type_id -> Int4,
        amount -> Nullable<Int4>,
        multiplier -> Nullable<Int4>,
        is_active -> Nullable<Bool>,
    }
}

table! {
    folder (id) {
        id -> Int4,
        name -> Text,
        location -> Text,
    }
}

table! {
    gamificationdocument (id) {
        id -> Int4,
    }
}

table! {
    gamificationdocumenttype (document_type_id) {
        document_type_id -> Int4,
        document_type_name -> Nullable<Text>,
    }
}

table! {
    gamificationpointtype (point_type_id) {
        point_type_id -> Int4,
        point_type_name -> Nullable<Text>,
    }
}

table! {
    icon (id) {
        id -> Int4,
        filename -> Nullable<Text>,
    }
}

table! {
    importedvelpgroups (user_group, doc_id, target_id, velp_group_id) {
        user_group -> Int4,
        doc_id -> Int4,
        target_type -> Int4,
        target_id -> Text,
        velp_group_id -> Int4,
    }
}

table! {
    labelinvelp (label_id, velp_id) {
        label_id -> Int4,
        velp_id -> Int4,
    }
}

table! {
    labelinvelpgroup (velp_group_id, group_label_id) {
        velp_group_id -> Int4,
        group_label_id -> Int4,
    }
}

table! {
    lecture (lecture_id) {
        lecture_id -> Int4,
        lecture_code -> Nullable<Text>,
        doc_id -> Int4,
        lecturer -> Int4,
        start_time -> Timestamptz,
        end_time -> Nullable<Timestamptz>,
        password -> Nullable<Text>,
        options -> Nullable<Text>,
    }
}

table! {
    lectureanswer (answer_id) {
        answer_id -> Int4,
        user_id -> Int4,
        question_id -> Int4,
        lecture_id -> Int4,
        answer -> Text,
        answered_on -> Timestamptz,
        points -> Nullable<Float8>,
    }
}

table! {
    lectureusers (lecture_id, user_id) {
        lecture_id -> Int4,
        user_id -> Int4,
    }
}

table! {
    message (msg_id) {
        msg_id -> Int4,
        lecture_id -> Int4,
        user_id -> Int4,
        #[sql_name = "message"]
        msg -> Text,
        timestamp -> Timestamptz,
    }
}

table! {
    newuser (email) {
        email -> Text,
        pass -> Text,
        created -> Timestamptz,
    }
}

table! {
    notification (user_id, doc_id) {
        user_id -> Int4,
        doc_id -> Int4,
        email_doc_modify -> Bool,
        email_comment_add -> Bool,
        email_comment_modify -> Bool,
    }
}

table! {
    printed_doc (id) {
        id -> Int4,
        doc_id -> Int4,
        template_doc_id -> Int4,
        file_type -> Text,
        path_to_file -> Nullable<Text>,
        version -> Text,
        temp -> Bool,
        created -> Timestamptz,
    }
}

table! {
    question (question_id) {
        question_id -> Int4,
        doc_id -> Int4,
        par_id -> Text,
        question_title -> Text,
        answer -> Nullable<Text>,
        questionjson -> Nullable<Text>,
        points -> Nullable<Text>,
        expl -> Nullable<Text>,
    }
}

table! {
    use diesel::sql_types::*;
    use super::QuestionactivitykindMapping;
    question_activity (asked_id, user_id, kind) {
        asked_id -> Integer,
        user_id -> Integer,
        kind -> QuestionactivitykindMapping,
    }
}

table! {
    use diesel::sql_types::*;
    use super::ReadparagraphtypeMapping;
    readparagraph (id) {
        id -> Integer,
        usergroup_id -> Integer,
        doc_id -> Nullable<Integer>,
        par_id -> Text,
        #[sql_name = "type"]
        type_ -> ReadparagraphtypeMapping,
        par_hash -> Text,
        timestamp -> Timestamptz,
    }
}

table! {
    use diesel::sql_types::*;
    use super::ReadparagraphtypeMapping;
    readparagraphs (usergroup_id, doc_id, par_id, type_) {
        usergroup_id -> Integer,
        doc_id -> Integer,
        par_id -> Text,
        #[sql_name = "type"]
        type_ -> ReadparagraphtypeMapping,
        par_hash -> Text,
        timestamp -> Timestamptz,
    }
}

table! {
    runningquestion (asked_id, lecture_id) {
        asked_id -> Int4,
        lecture_id -> Int4,
        ask_time -> Timestamptz,
        end_time -> Nullable<Timestamptz>,
    }
}

table! {
    showpoints (asked_id) {
        asked_id -> Int4,
    }
}

table! {
    slide_status (doc_id) {
        doc_id -> Int4,
        status -> Text,
    }
}

table! {
    translation (doc_id) {
        doc_id -> Int4,
        src_docid -> Int4,
        lang_id -> Text,
    }
}

table! {
    useraccount (id) {
        id -> Int4,
        name -> Text,
        real_name -> Nullable<Text>,
        email -> Nullable<Text>,
        prefs -> Nullable<Text>,
        pass -> Nullable<Text>,
        yubikey -> Nullable<Text>,
    }
}

table! {
    useractivity (lecture_id, user_id) {
        lecture_id -> Int4,
        user_id -> Int4,
        active -> Timestamptz,
    }
}

table! {
    useranswer (id) {
        id -> Int4,
        answer_id -> Int4,
        user_id -> Int4,
    }
}

table! {
    usergamification (gamification_doc_id, user_id) {
        gamification_doc_id -> Int4,
        user_id -> Int4,
        is_gamified -> Nullable<Bool>,
    }
}

table! {
    usergroup (id) {
        id -> Int4,
        name -> Text,
    }
}

table! {
    usergroupmember (usergroup_id, user_id) {
        usergroup_id -> Int4,
        user_id -> Int4,
    }
}

table! {
    usernotes (id) {
        id -> Int4,
        usergroup_id -> Int4,
        doc_id -> Int4,
        par_id -> Text,
        par_hash -> Text,
        content -> Text,
        created -> Timestamptz,
        modified -> Nullable<Timestamptz>,
        access -> Text,
        tags -> Text,
        html -> Nullable<Text>,
    }
}

table! {
    velp (id) {
        id -> Int4,
        creator_id -> Int4,
        creation_time -> Timestamptz,
        default_points -> Nullable<Float8>,
        icon_id -> Nullable<Int4>,
        valid_from -> Nullable<Timestamptz>,
        valid_until -> Nullable<Timestamptz>,
        color -> Nullable<Text>,
        visible_to -> Int4,
    }
}

table! {
    velpcontent (version_id, language_id) {
        version_id -> Int4,
        language_id -> Text,
        content -> Nullable<Text>,
        default_comment -> Nullable<Text>,
    }
}

table! {
    velpgroup (id) {
        id -> Int4,
        name -> Nullable<Text>,
        creation_time -> Timestamptz,
        valid_from -> Nullable<Timestamptz>,
        valid_until -> Nullable<Timestamptz>,
        default_group -> Nullable<Bool>,
    }
}

table! {
    velpgroupdefaults (doc_id, target_id, velp_group_id) {
        doc_id -> Int4,
        target_type -> Int4,
        target_id -> Text,
        velp_group_id -> Int4,
        selected -> Nullable<Bool>,
    }
}

table! {
    velpgrouplabel (id) {
        id -> Int4,
        content -> Text,
    }
}

table! {
    velpgroupselection (user_id, doc_id, target_id, velp_group_id) {
        user_id -> Int4,
        doc_id -> Int4,
        target_type -> Int4,
        target_id -> Text,
        selected -> Nullable<Bool>,
        velp_group_id -> Int4,
    }
}

table! {
    velpgroupsindocument (user_id, doc_id, velp_group_id) {
        user_id -> Int4,
        doc_id -> Int4,
        velp_group_id -> Int4,
    }
}

table! {
    velpingroup (velp_group_id, velp_id) {
        velp_group_id -> Int4,
        velp_id -> Int4,
        points -> Nullable<Float8>,
    }
}

table! {
    velplabel (id) {
        id -> Int4,
    }
}

table! {
    velplabelcontent (velplabel_id, language_id) {
        velplabel_id -> Int4,
        language_id -> Text,
        content -> Nullable<Text>,
    }
}

table! {
    velpversion (id) {
        id -> Int4,
        velp_id -> Int4,
        modify_time -> Timestamptz,
    }
}

joinable!(annotation -> answer (answer_id));
joinable!(annotation -> block (document_id));
joinable!(annotation -> icon (icon_id));
joinable!(annotation -> useraccount (annotator_id));
joinable!(annotation -> velpversion (velp_version_id));
joinable!(annotationcomment -> annotation (annotation_id));
joinable!(annotationcomment -> useraccount (commenter_id));
joinable!(answer -> usergroup (last_points_modifier));
joinable!(answertag -> answer (answer_id));
joinable!(answerupload -> answer (answer_id));
joinable!(answerupload -> block (upload_block_id));
joinable!(askedquestion -> askedjson (asked_json_id));
joinable!(askedquestion -> block (doc_id));
joinable!(askedquestion -> lecture (lecture_id));
joinable!(blockaccess -> accesstype (type_));
joinable!(blockaccess -> block (block_id));
joinable!(blockaccess -> usergroup (usergroup_id));
joinable!(docentry -> block (id));
joinable!(docgamified -> gamificationdocumenttype (doc_type_id));
joinable!(documentgamificationpoint -> block (doc_id));
joinable!(documentgamificationpoint -> gamificationpointtype (point_type_id));
joinable!(folder -> block (id));
joinable!(gamificationdocument -> block (id));
joinable!(importedvelpgroups -> block (doc_id));
joinable!(importedvelpgroups -> usergroup (user_group));
joinable!(importedvelpgroups -> velpgroup (velp_group_id));
joinable!(labelinvelp -> velp (velp_id));
joinable!(labelinvelp -> velplabel (label_id));
joinable!(labelinvelpgroup -> velpgroup (velp_group_id));
joinable!(labelinvelpgroup -> velpgrouplabel (group_label_id));
joinable!(lecture -> block (doc_id));
joinable!(lecture -> useraccount (lecturer));
joinable!(lectureanswer -> askedquestion (question_id));
joinable!(lectureanswer -> lecture (lecture_id));
joinable!(lectureanswer -> useraccount (user_id));
joinable!(lectureusers -> lecture (lecture_id));
joinable!(lectureusers -> useraccount (user_id));
joinable!(message -> lecture (lecture_id));
joinable!(message -> useraccount (user_id));
joinable!(notification -> block (doc_id));
joinable!(notification -> useraccount (user_id));
joinable!(question -> block (doc_id));
joinable!(question_activity -> askedquestion (asked_id));
joinable!(question_activity -> useraccount (user_id));
joinable!(readparagraph -> block (doc_id));
joinable!(readparagraphs -> block (doc_id));
joinable!(runningquestion -> askedquestion (asked_id));
joinable!(runningquestion -> lecture (lecture_id));
joinable!(showpoints -> askedquestion (asked_id));
joinable!(slide_status -> block (doc_id));
joinable!(useractivity -> lecture (lecture_id));
joinable!(useractivity -> useraccount (user_id));
joinable!(useranswer -> answer (answer_id));
joinable!(useranswer -> useraccount (user_id));
joinable!(usergamification -> gamificationdocument (gamification_doc_id));
joinable!(usergamification -> useraccount (user_id));
joinable!(usergroupmember -> useraccount (user_id));
joinable!(usergroupmember -> usergroup (usergroup_id));
joinable!(usernotes -> block (doc_id));
joinable!(usernotes -> usergroup (usergroup_id));
joinable!(velp -> icon (icon_id));
joinable!(velp -> useraccount (creator_id));
joinable!(velpcontent -> velpversion (version_id));
joinable!(velpgroupdefaults -> block (doc_id));
joinable!(velpgroupdefaults -> velpgroup (velp_group_id));
joinable!(velpgroupselection -> block (doc_id));
joinable!(velpgroupselection -> useraccount (user_id));
joinable!(velpgroupselection -> velpgroup (velp_group_id));
joinable!(velpgroupsindocument -> block (doc_id));
joinable!(velpgroupsindocument -> useraccount (user_id));
joinable!(velpgroupsindocument -> velpgroup (velp_group_id));
joinable!(velpingroup -> velp (velp_id));
joinable!(velpingroup -> velpgroup (velp_group_id));
joinable!(velplabelcontent -> velplabel (velplabel_id));
joinable!(velpversion -> velp (velp_id));

allow_tables_to_appear_in_same_query!(
    accesstype,
    alembic_version,
    annotation,
    annotationcomment,
    answer,
    answertag,
    answerupload,
    askedjson,
    askedquestion,
    block,
    blockaccess,
    docentry,
    docgamified,
    documentgamificationpoint,
    folder,
    gamificationdocument,
    gamificationdocumenttype,
    gamificationpointtype,
    icon,
    importedvelpgroups,
    labelinvelp,
    labelinvelpgroup,
    lecture,
    lectureanswer,
    lectureusers,
    message,
    newuser,
    notification,
    printed_doc,
    question,
    question_activity,
    readparagraph,
    readparagraphs,
    runningquestion,
    showpoints,
    slide_status,
    translation,
    useraccount,
    useractivity,
    useranswer,
    usergamification,
    usergroup,
    usergroupmember,
    usernotes,
    velp,
    velpcontent,
    velpgroup,
    velpgroupdefaults,
    velpgrouplabel,
    velpgroupselection,
    velpgroupsindocument,
    velpingroup,
    velplabel,
    velplabelcontent,
    velpversion,
);
