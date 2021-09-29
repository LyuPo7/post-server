#!/bin/bash
### Categories
## getCategories
curl "http://localhost:3000/getCategories?token=2a874271-f5e5-451b-bb9f-fe6a281414bf"
## createCategory
curl "http://localhost:3000/createCategory?title=sport&token=2a874271-f5e5-451b-bb9f-fe6a281414bf"
curl "http://localhost:3000/createCategory?title=crossfit&token=2a874271-f5e5-451b-bb9f-fe6a281414bf"
curl "http://localhost:3000/createCategory?title=box&token=2a874271-f5e5-451b-bb9f-fe6a281414bf"
## createCategory
curl "http://localhost:3000/createCategory?title=sport&subcategory=sport&token=2a874271-f5e5-451b-bb9f-fe6a281414bf"
curl "http://localhost:3000/createCategory?title=crossfit&subcategory=sport&token=2a874271-f5e5-451b-bb9f-fe6a281414bf"
curl "http://localhost:3000/createCategory?title=box&subcategory=sport&token=2a874271-f5e5-451b-bb9f-fe6a281414bf"
