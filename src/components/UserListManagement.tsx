'use client';

import React, { useState, useEffect, useCallback } from 'react';
import { User, PaginatedUsersResponse, UserFilters, APIResponse } from '@/types/user';
import { userService } from '@/services/userService';
import { Button } from '@/components/ui/Button';
import { Input } from '@/components/ui/Input';
import { Select } from '@/components/ui/Select';

interface UserListManagementProps {
  onUserSelect?: (user: User, action: 'update' | 'delete') => void;
  onExit?: () => void;
  className?: string;
}

export const UserListManagement: React.FC<UserListManagementProps> = ({
  onUserSelect,
  onExit,
  className = '',
}) => {
  const [users, setUsers] = useState<User[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [selectedUserId, setSelectedUserId] = useState<string>('');
  const [searchUserId, setSearchUserId] = useState<string>('');
  const [currentPage, setCurrentPage] = useState(1);
  const [totalPages, setTotalPages] = useState(1);
  const [hasNext, setHasNext] = useState(false);
  const [hasPrev, setHasPrev] = useState(false);
  const [filters, setFilters] = useState<UserFilters>({});

  const ITEMS_PER_PAGE = 10;

  const fetchUsers = useCallback(async (page: number = 1, userIdFilter?: string) => {
    setLoading(true);
    setError(null);

    try {
      const searchParams = {
        page,
        limit: ITEMS_PER_PAGE,
        sortBy: 'userId',
        sortOrder: 'asc' as const,
        filters: {
          ...filters,
          ...(userIdFilter ? { search: userIdFilter } : {}),
        },
      };

      const response: APIResponse<PaginatedUsersResponse> = await userService.listUsers(searchParams);

      if (response.success && response.data) {
        setUsers(response.data.data);
        setCurrentPage(response.data.pagination.page);
        setTotalPages(response.data.pagination.totalPages);
        setHasNext(response.data.pagination.hasNext);
        setHasPrev(response.data.pagination.hasPrev);
      } else {
        setError(response.error?.message || 'Failed to fetch users');
        setUsers([]);
      }
    } catch (err) {
      setError('Network error occurred while fetching users');
      setUsers([]);
    } finally {
      setLoading(false);
    }
  }, [filters]);

  useEffect(() => {
    fetchUsers(1);
  }, [fetchUsers]);

  useEffect(() => {
    const handleKeyDown = (event: KeyboardEvent) => {
      switch (event.key) {
        case 'Enter':
          event.preventDefault();
          handleSearch();
          break;
        case 'F3':
          event.preventDefault();
          handleExit();
          break;
        case 'F7':
          event.preventDefault();
          handlePreviousPage();
          break;
        case 'F8':
          event.preventDefault();
          handleNextPage();
          break;
        case 'u':
        case 'U':
          if (selectedUserId) {
            event.preventDefault();
            handleUserAction('update');
          }
          break;
        case 'd':
        case 'D':
          if (selectedUserId) {
            event.preventDefault();
            handleUserAction('delete');
          }
          break;
      }
    };

    document.addEventListener('keydown', handleKeyDown);
    return () => {
      document.removeEventListener('keydown', handleKeyDown);
    };
  }, [selectedUserId, currentPage, hasNext, hasPrev]);

  const handleSearch = () => {
    if (searchUserId.trim()) {
      fetchUsers(1, searchUserId.trim());
    } else {
      fetchUsers(1);
    }
  };

  const handleClearSearch = () => {
    setSearchUserId('');
    fetchUsers(1);
  };

  const handleNextPage = () => {
    if (hasNext) {
      fetchUsers(currentPage + 1, searchUserId || undefined);
    }
  };

  const handlePreviousPage = () => {
    if (hasPrev) {
      fetchUsers(currentPage - 1, searchUserId || undefined);
    }
  };

  const handleUserAction = (action: 'update' | 'delete') => {
    const user = users.find(u => u.userId === selectedUserId);
    if (user && onUserSelect) {
      onUserSelect(user, action);
    }
  };

  const handleExit = () => {
    if (onExit) {
      onExit();
    }
  };

  const handleUserTypeFilterChange = (value: string) => {
    const newFilters = { ...filters };
    if (value === 'all') {
      delete newFilters.userType;
    } else {
      newFilters.userType = value as 'A' | 'U';
    }
    setFilters(newFilters);
  };

  const formatUserType = (userType: 'A' | 'U'): string => {
    return userType === 'A' ? 'Admin' : 'User';
  };

  if (loading) {
    return (
      <div className={`bg-background border border-border rounded-lg p-6 ${className}`}>
        <div className="flex items-center justify-center h-64">
          <div className="text-muted-foreground">Loading users...</div>
        </div>
      </div>
    );
  }

  return (
    <div className={`bg-background border border-border rounded-lg p-6 ${className}`}>
      {/* Header */}
      <div className="mb-6">
        <h1 className="text-2xl font-bold text-foreground mb-2">User List Management - COUSR00C</h1>
        <p className="text-sm text-muted-foreground">
          Use ENTER to search, PF3 to exit, PF7/PF8 for navigation, U/D for actions
        </p>
      </div>

      {/* Search and Filter Controls */}
      <div className="mb-6 space-y-4">
        <div className="flex gap-4 items-end">
          <div className="flex-1">
            <Input
              label="Search by User ID"
              value={searchUserId}
              onChange={(e) => setSearchUserId(e.target.value.toUpperCase())}
              placeholder="Enter User ID..."
              maxLength={8}
              className="font-mono"
            />
          </div>
          <div className="w-48">
            <Select
              label="User Type"
              value={filters.userType || 'all'}
              onChange={handleUserTypeFilterChange}
              options={[
                { value: 'all', label: 'All Types' },
                { value: 'A', label: 'Admin' },
                { value: 'U', label: 'User' },
              ]}
            />
          </div>
          <Button onClick={handleSearch} variant="primary">
            Search (ENTER)
          </Button>
          <Button onClick={handleClearSearch} variant="outline">
            Clear
          </Button>
        </div>
      </div>

      {/* Error Display */}
      {error && (
        <div className="mb-4 p-3 bg-destructive/10 border border-destructive/20 rounded-md">
          <p className="text-destructive text-sm">{error}</p>
        </div>
      )}

      {/* User Table */}
      <div className="mb-6">
        <div className="border border-border rounded-lg overflow-hidden">
          <table className="w-full">
            <thead className="bg-muted">
              <tr>
                <th className="px-4 py-3 text-left text-sm font-medium text-foreground border-b border-border">
                  Select
                </th>
                <th className="px-4 py-3 text-left text-sm font-medium text-foreground border-b border-border">
                  User ID
                </th>
                <th className="px-4 py-3 text-left text-sm font-medium text-foreground border-b border-border">
                  First Name
                </th>
                <th className="px-4 py-3 text-left text-sm font-medium text-foreground border-b border-border">
                  Last Name
                </th>
                <th className="px-4 py-3 text-left text-sm font-medium text-foreground border-b border-border">
                  User Type
                </th>
              </tr>
            </thead>
            <tbody>
              {users.length === 0 ? (
                <tr>
                  <td colSpan={5} className="px-4 py-8 text-center text-muted-foreground">
                    No users found
                  </td>
                </tr>
              ) : (
                users.map((user, index) => (
                  <tr
                    key={user.userId}
                    className={`border-b border-border hover:bg-muted/50 ${
                      selectedUserId === user.userId ? 'bg-primary/10' : ''
                    }`}
                  >
                    <td className="px-4 py-3">
                      <input
                        type="radio"
                        name="selectedUser"
                        value={user.userId}
                        checked={selectedUserId === user.userId}
                        onChange={(e) => setSelectedUserId(e.target.value)}
                        className="w-4 h-4 text-primary focus:ring-primary"
                      />
                    </td>
                    <td className="px-4 py-3 font-mono text-sm text-foreground">
                      {user.userId}
                    </td>
                    <td className="px-4 py-3 text-sm text-foreground">
                      {user.firstName}
                    </td>
                    <td className="px-4 py-3 text-sm text-foreground">
                      {user.lastName}
                    </td>
                    <td className="px-4 py-3 text-sm text-foreground">
                      <span className={`px-2 py-1 rounded-full text-xs font-medium ${
                        user.userType === 'A' 
                          ? 'bg-primary/10 text-primary' 
                          : 'bg-secondary/10 text-secondary-foreground'
                      }`}>
                        {formatUserType(user.userType)}
                      </span>
                    </td>
                  </tr>
                ))
              )}
            </tbody>
          </table>
        </div>
      </div>

      {/* Pagination Info */}
      <div className="mb-4 text-sm text-muted-foreground">
        Page {currentPage} of {totalPages} ({users.length} users displayed, {ITEMS_PER_PAGE} per page)
      </div>

      {/* Action Buttons */}
      <div className="flex justify-between items-center">
        <div className="flex gap-2">
          <Button
            onClick={handlePreviousPage}
            disabled={!hasPrev}
            variant="outline"
            size="sm"
          >
            ← Previous (PF7)
          </Button>
          <Button
            onClick={handleNextPage}
            disabled={!hasNext}
            variant="outline"
            size="sm"
          >
            Next (PF8) →
          </Button>
        </div>

        <div className="flex gap-2">
          <Button
            onClick={() => handleUserAction('update')}
            disabled={!selectedUserId}
            variant="primary"
            size="sm"
          >
            Update (U)
          </Button>
          <Button
            onClick={() => handleUserAction('delete')}
            disabled={!selectedUserId}
            variant="destructive"
            size="sm"
          >
            Delete (D)
          </Button>
          <Button
            onClick={handleExit}
            variant="outline"
            size="sm"
          >
            Exit (PF3)
          </Button>
        </div>
      </div>

      {/* Selected User Info */}
      {selectedUserId && (
        <div className="mt-4 p-3 bg-muted/50 rounded-md">
          <p className="text-sm text-foreground">
            Selected User: <span className="font-mono font-medium">{selectedUserId}</span>
            {users.find(u => u.userId === selectedUserId) && (
              <span className="ml-2">
                - {users.find(u => u.userId === selectedUserId)?.firstName} {users.find(u => u.userId === selectedUserId)?.lastName}
              </span>
            )}
          </p>
        </div>
      )}

      {/* Keyboard Shortcuts Help */}
      <div className="mt-6 pt-4 border-t border-border">
        <p className="text-xs text-muted-foreground">
          <strong>Keyboard Shortcuts:</strong> ENTER = Search | PF3 = Exit | PF7 = Previous Page | PF8 = Next Page | U = Update Selected | D = Delete Selected
        </p>
      </div>
    </div>
  );
};

export default UserListManagement;