'use client';

import { useState, useEffect, useCallback } from 'react';
import { Button } from '@/components/ui/Button';
import { Input } from '@/components/ui/Input';
import { Table } from '@/components/ui/Table';
import { userService } from '@/services/userService';
import { 
  UserResponse, 
  UserListResponse, 
  UserSearchRequest, 
  PaginationInfo,
  UserType,
  USER_TYPE_LABELS 
} from '@/types/user';

interface UserListProps {
  onUserSelect?: (user: UserResponse) => void;
  onUserUpdate?: (user: UserResponse) => void;
  onUserDelete?: (userId: string) => void;
  className?: string;
}

export default function UserList({ 
  onUserSelect, 
  onUserUpdate, 
  onUserDelete, 
  className = '' 
}: UserListProps) {
  const [users, setUsers] = useState<UserResponse[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [pagination, setPagination] = useState<PaginationInfo>({
    currentPage: 1,
    totalPages: 1,
    totalItems: 0,
    itemsPerPage: 10,
    hasNextPage: false,
    hasPreviousPage: false,
  });
  const [selectedUserIndex, setSelectedUserIndex] = useState<number>(-1);
  const [userIdFilter, setUserIdFilter] = useState<string>('');
  const [searchTerm, setSearchTerm] = useState<string>('');

  const fetchUsers = useCallback(async (searchParams: UserSearchRequest = {}) => {
    try {
      setLoading(true);
      setError(null);
      
      const params: UserSearchRequest = {
        page: pagination.currentPage,
        limit: 10,
        ...searchParams,
      };

      if (userIdFilter.trim()) {
        params.userId = userIdFilter.trim().toUpperCase();
      }

      if (searchTerm.trim()) {
        params.searchTerm = searchTerm.trim();
      }

      const response: UserListResponse = await userService.listUsers(params);
      
      setUsers(response.users || []);
      setPagination(response.pagination);
      setSelectedUserIndex(-1);
    } catch (err) {
      const errorMessage = err instanceof Error ? err.message : 'Failed to fetch users';
      setError(errorMessage);
      setUsers([]);
    } finally {
      setLoading(false);
    }
  }, [pagination.currentPage, userIdFilter, searchTerm]);

  const handlePageChange = useCallback((newPage: number) => {
    setPagination(prev => ({ ...prev, currentPage: newPage }));
  }, []);

  const handlePreviousPage = useCallback(() => {
    if (pagination.hasPreviousPage) {
      handlePageChange(pagination.currentPage - 1);
    }
  }, [pagination.hasPreviousPage, pagination.currentPage, handlePageChange]);

  const handleNextPage = useCallback(() => {
    if (pagination.hasNextPage) {
      handlePageChange(pagination.currentPage + 1);
    }
  }, [pagination.hasNextPage, pagination.currentPage, handlePageChange]);

  const handleUserIdFilterChange = useCallback((value: string) => {
    setUserIdFilter(value);
    setPagination(prev => ({ ...prev, currentPage: 1 }));
  }, []);

  const handleSearchChange = useCallback((value: string) => {
    setSearchTerm(value);
    setPagination(prev => ({ ...prev, currentPage: 1 }));
  }, []);

  const handleKeyDown = useCallback((event: KeyboardEvent) => {
    switch (event.key) {
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
        if (selectedUserIndex >= 0 && users[selectedUserIndex]) {
          event.preventDefault();
          onUserUpdate?.(users[selectedUserIndex]);
        }
        break;
      case 'd':
      case 'D':
        if (selectedUserIndex >= 0 && users[selectedUserIndex]) {
          event.preventDefault();
          onUserDelete?.(users[selectedUserIndex].userId);
        }
        break;
      case 'ArrowUp':
        event.preventDefault();
        setSelectedUserIndex(prev => Math.max(0, prev - 1));
        break;
      case 'ArrowDown':
        event.preventDefault();
        setSelectedUserIndex(prev => Math.min(users.length - 1, prev + 1));
        break;
      case 'Enter':
        if (selectedUserIndex >= 0 && users[selectedUserIndex]) {
          event.preventDefault();
          onUserSelect?.(users[selectedUserIndex]);
        }
        break;
    }
  }, [selectedUserIndex, users, handlePreviousPage, handleNextPage, onUserUpdate, onUserDelete, onUserSelect]);

  const handleUserClick = useCallback((user: UserResponse, index: number) => {
    setSelectedUserIndex(index);
    onUserSelect?.(user);
  }, [onUserSelect]);

  const handleUpdateClick = useCallback((user: UserResponse) => {
    const index = users.findIndex(u => u.userId === user.userId);
    setSelectedUserIndex(index);
    onUserUpdate?.(user);
  }, [users, onUserUpdate]);

  const handleDeleteClick = useCallback((userId: string) => {
    const index = users.findIndex(u => u.userId === userId);
    setSelectedUserIndex(index);
    onUserDelete?.(userId);
  }, [users, onUserDelete]);

  const refreshUsers = useCallback(() => {
    fetchUsers();
  }, [fetchUsers]);

  useEffect(() => {
    fetchUsers();
  }, [fetchUsers]);

  useEffect(() => {
    const handleGlobalKeyDown = (event: KeyboardEvent) => {
      handleKeyDown(event);
    };

    window.addEventListener('keydown', handleGlobalKeyDown);
    return () => {
      window.removeEventListener('keydown', handleGlobalKeyDown);
    };
  }, [handleKeyDown]);

  const tableColumns = [
    { key: 'userId' as const, label: 'User ID', sortable: true },
    { key: 'firstName' as const, label: 'First Name', sortable: true },
    { key: 'lastName' as const, label: 'Last Name', sortable: true },
    { key: 'userType' as const, label: 'User Type', sortable: true },
    { key: 'actions' as const, label: 'Actions', sortable: false },
  ];

  const tableData = users.map((user, index) => ({
    ...user,
    userType: USER_TYPE_LABELS[user.userType as UserType] || user.userType,
    actions: (
      <div className="flex gap-2">
        <Button
          size="sm"
          variant="outline"
          onClick={() => handleUpdateClick(user)}
          className="text-xs"
        >
          Update (U)
        </Button>
        <Button
          size="sm"
          variant="destructive"
          onClick={() => handleDeleteClick(user.userId)}
          className="text-xs"
        >
          Delete (D)
        </Button>
      </div>
    ),
    className: selectedUserIndex === index ? 'bg-blue-50 border-blue-200' : '',
    onClick: () => handleUserClick(user, index),
  }));

  if (loading && users.length === 0) {
    return (
      <div className={`flex items-center justify-center p-8 ${className}`}>
        <div className="text-center">
          <div className="animate-spin rounded-full h-8 w-8 border-b-2 border-primary mx-auto mb-4"></div>
          <p className="text-muted-foreground">Loading users...</p>
        </div>
      </div>
    );
  }

  return (
    <div className={`space-y-6 ${className}`}>
      {/* Header */}
      <div className="flex items-center justify-between">
        <h2 className="text-2xl font-bold">User List (COUSR00C)</h2>
        <Button onClick={refreshUsers} variant="outline" disabled={loading}>
          Refresh
        </Button>
      </div>

      {/* Filters */}
      <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
        <Input
          label="Filter by User ID"
          value={userIdFilter}
          onChange={(e) => handleUserIdFilterChange(e.target.value)}
          placeholder="Enter User ID..."
          className="max-w-xs"
        />
        <Input
          label="Search Users"
          value={searchTerm}
          onChange={(e) => handleSearchChange(e.target.value)}
          placeholder="Search by name or ID..."
          className="max-w-xs"
        />
      </div>

      {/* Error Message */}
      {error && (
        <div className="bg-destructive/10 border border-destructive/20 rounded-md p-4">
          <p className="text-destructive font-medium">Error: {error}</p>
          <Button 
            onClick={refreshUsers} 
            variant="outline" 
            size="sm" 
            className="mt-2"
          >
            Retry
          </Button>
        </div>
      )}

      {/* Users Table */}
      {!error && (
        <div className="border rounded-lg overflow-hidden">
          <Table
            columns={tableColumns}
            data={tableData}
            loading={loading}
            emptyMessage="No users found"
            className="w-full"
          />
        </div>
      )}

      {/* Pagination */}
      {!error && users.length > 0 && (
        <div className="flex items-center justify-between">
          <div className="text-sm text-muted-foreground">
            Showing {((pagination.currentPage - 1) * pagination.itemsPerPage) + 1} to{' '}
            {Math.min(pagination.currentPage * pagination.itemsPerPage, pagination.totalItems)} of{' '}
            {pagination.totalItems} users
          </div>
          
          <div className="flex items-center gap-2">
            <Button
              variant="outline"
              size="sm"
              onClick={handlePreviousPage}
              disabled={!pagination.hasPreviousPage || loading}
            >
              Previous (F7)
            </Button>
            
            <span className="text-sm px-3 py-1 bg-muted rounded">
              Page {pagination.currentPage} of {pagination.totalPages}
            </span>
            
            <Button
              variant="outline"
              size="sm"
              onClick={handleNextPage}
              disabled={!pagination.hasNextPage || loading}
            >
              Next (F8)
            </Button>
          </div>
        </div>
      )}

      {/* Navigation Instructions */}
      <div className="text-xs text-muted-foreground bg-muted/50 p-3 rounded-md">
        <p className="font-medium mb-1">Navigation Instructions:</p>
        <div className="grid grid-cols-2 md:grid-cols-4 gap-2">
          <span>F7: Previous Page</span>
          <span>F8: Next Page</span>
          <span>U: Update Selected</span>
          <span>D: Delete Selected</span>
          <span>↑/↓: Navigate Users</span>
          <span>Enter: Select User</span>
        </div>
      </div>

      {/* Selected User Info */}
      {selectedUserIndex >= 0 && users[selectedUserIndex] && (
        <div className="bg-primary/5 border border-primary/20 rounded-md p-4">
          <h3 className="font-medium mb-2">Selected User:</h3>
          <div className="grid grid-cols-2 md:grid-cols-4 gap-4 text-sm">
            <div>
              <span className="font-medium">ID:</span> {users[selectedUserIndex].userId}
            </div>
            <div>
              <span className="font-medium">Name:</span> {users[selectedUserIndex].firstName} {users[selectedUserIndex].lastName}
            </div>
            <div>
              <span className="font-medium">Type:</span> {USER_TYPE_LABELS[users[selectedUserIndex].userType as UserType]}
            </div>
            <div>
              <span className="font-medium">Created:</span> {new Date(users[selectedUserIndex].createdAt).toLocaleDateString()}
            </div>
          </div>
        </div>
      )}
    </div>
  );
}