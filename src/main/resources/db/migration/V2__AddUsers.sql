CREATE TABLE USERS (
  ID BIGSERIAL PRIMARY KEY,
  USER_NAME VARCHAR NOT NULL UNIQUE,
  EMAIL VARCHAR NOT NULL,
  HASH VARCHAR NOT NULL,
  IS_ADMIN BOOLEAN NOT NULL
);