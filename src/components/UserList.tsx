'use client';

import React from 'react';
import { Table, Button } from '@/components/ui';
import { User } from '@/types/user';

interface UserListProps {
  users: User[];
  loading?: boolean;
  currentPage: number;
  totalPages: number;
  hasNext: boolean;
  hasPrevious: boolean;
  onPageChange: (page: number) => void;
  onEdit: (user: User) => void;
  onDelete: (userId: string) => void;
}

export default function UserList({
  users,
  loading = false,
  currentPage,
  totalPages,
  hasNext,
  hasPrevious,
  onPageChange,
  onEdit,
  onDelete,
}: UserListProps) {
  const handlePrevious = () => {
    if (hasPrevious) {
      onPageChange(currentPage - 1);
    }
  };

  const handleNext = () => {
    if (hasNext) {
      onPageChange(currentPage + 1);
    }
  };

  const handleDelete = (userId: string, e: React.MouseEvent) => {
    e.stopPropagation();
    if (confirm('Are you sure you want to delete this user?')) {
      onDelete(userId);
    }
  };

  const handleEdit = (user: User, e: React.MouseEvent) => {
    e.stopPropagation();
    onEdit(user);
  };

  const formatUserType = (userType: string) => {
    return userType === 'A' ? 'Admin' : 'Regular';
  };

  if (loading) {
    return (
      <div className="flex items-center justify-center p-8">
        <div className="text-gray-600">Loading users...</div>
      </div>
    );
  }

  if (!users || users.length === 0) {
    return (
      <div className="flex flex-col items-center justify-center p-8 bg-gray-50 rounded-lg border border-gray-200">
        <svg
          className="w-16 h-16 text-gray-400 mb-4"
          fill="none"
          stroke="currentColor"
          viewBox="0 0 24 24"
          xmlns="http://www.w3.org/2000/svg"
        >
          <path
            strokeLinecap="round"
            strokeLinejoin="round"
            strokeWidth={2}
            d="M12 4.354a4 4 0 110 5.292M15 21H3v-1a6 6 0 0112 0v1zm0 0h6v-1a6 6 0 00-9-5.197M13 7a4 4 0 11-8 0 4 4 0 018 0z"
          />
        </svg>
        <h3 className="text-lg font-semibold text-gray-900 mb-2">No users found</h3>
        <p className="text-gray-600 text-center">
          There are no users to display. Create a new user to get started.
        </p>
      </div>
    );
  }

  return (
    <div className="space-y-4">
      <Table
        columns={[
          { key: 'userId', label: 'User ID' },
          { key: 'firstName', label: 'First Name' },
          { key: 'lastName', label: 'Last Name' },
          { 
            key: 'userType', 
            label: 'User Type',
            render: (value) => formatUserType(value as string)
          },
        ]}
        data={users}
        actions={(user) => (
          <div className="flex gap-2">
            <Button
              size="sm"
              onClick={(e) => handleEdit(user, e)}
            >
              Edit
            </Button>
            <Button
              size="sm"
              variant="danger"
              onClick={(e) => handleDelete(user.userId, e)}
            >
              Delete
            </Button>
          </div>
        )}
      />

      <div className="flex items-center justify-between border-t border-gray-200 bg-white px-4 py-3 sm:px-6 rounded-b-lg">
        <div className="flex flex-1 justify-between sm:hidden">
          <Button
            variant="secondary"
            size="sm"
            onClick={handlePrevious}
            disabled={!hasPrevious}
          >
            Previous
          </Button>
          <Button
            variant="secondary"
            size="sm"
            onClick={handleNext}
            disabled={!hasNext}
          >
            Next
          </Button>
        </div>
        <div className="hidden sm:flex sm:flex-1 sm:items-center sm:justify-between">
          <div>
            <p className="text-sm text-gray-700">
              Page <span className="font-medium">{currentPage + 1}</span> of{' '}
              <span className="font-medium">{totalPages}</span>
            </p>
          </div>
          <div className="flex gap-2">
            <Button
              variant="secondary"
              size="sm"
              onClick={handlePrevious}
              disabled={!hasPrevious}
            >
              Previous
            </Button>
            <Button
              variant="secondary"
              size="sm"
              onClick={handleNext}
              disabled={!hasNext}
            >
              Next
            </Button>
          </div>
        </div>
      </div>
    </div>
  );
}
