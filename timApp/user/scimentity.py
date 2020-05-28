from datetime import datetime
from typing import Dict

from flask import current_app

DEFAULT_TIMESTAMP = datetime(
    year=2015,
    month=1,
    day=1,
)


class SCIMEntity:
    @property
    def scim_created(self):
        raise NotImplementedError

    @property
    def scim_modified(self):
        raise NotImplementedError

    @property
    def scim_id(self):
        raise NotImplementedError

    @property
    def scim_resource_type(self):
        raise NotImplementedError

    @property
    def scim_display_name(self):
        raise NotImplementedError

    @property
    def scim_location(self):
        host = current_app.config['TIM_HOST']
        return f'{host}/scim/{self.scim_resource_type}s/{self.scim_id}'

    @property
    def scim_extra_data(self):
        return {}

    def get_scim_data(self) -> Dict:
        return {
            'schemas': [f"urn:ietf:params:scim:schemas:core:2.0:{self.scim_resource_type}"],
            'id': self.scim_id,
            'externalId': self.scim_id,
            'meta': get_meta(self),
            'displayName': self.scim_display_name,
            **self.scim_extra_data,
        }


def get_meta(g: SCIMEntity):
    scim_type = g.scim_resource_type
    host = current_app.config['TIM_HOST']
    return {
        'created': g.scim_created or DEFAULT_TIMESTAMP,
        'lastModified': g.scim_modified or DEFAULT_TIMESTAMP,
        'location': f'{host}/scim/{scim_type}s/{g.scim_id}',
        'resourceType': scim_type,
        # 'version': '',
    }
