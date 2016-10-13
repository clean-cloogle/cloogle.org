#!/bin/bash
set -e
docker build -t cloogle-backend backend
docker build -t cloogle-frontend frontend
docker build -t cloogle-stats frontend/stats
