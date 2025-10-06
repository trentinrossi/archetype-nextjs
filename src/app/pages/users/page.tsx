'use client';

import { useState, useEffect, useCallback, useRef } from 'react';
import Link from 'next/link';
import { Button } from '@/components/ui/Button';
import { Input } from '@/components/ui/Input';
import { Select } from '@/components/ui/Select';
import { Table, TableHeader, TableBody, TableRow, TableHead, TableCell } from '@/components/ui/Table';
import { Modal } from '@/components/ui/Modal';
import { userService } from '@/services/userService';
import UserForm from '@/components/UserForm';
import {
  UserResponse,
  UserSearchRequest,
  PaginationInfo,
  UserType,
  USER_TYPE_LABELS,
  DEFAULT_PAGINATION,
  CreateUserRequest,
  UpdateUserRequest,
} from '@/types/user';

interface BreadcrumbItem {
  label: string;
  href?: string;
}

interface UserListState {
  users: UserResponse[];
  loading: boolean;
  error: string | null;
  pagination: PaginationInfo | null;
  searchParams: UserSearchRequest;
}

interface ModalState {
  isOpen: boolean;
  mode: 'create' | 'edit' | 'view' | 'delete' | null;
  user: UserResponse | null;
}

export default function UsersPage() {
  // State management
  const [state, setState] = useState<UserListState>({
    users: [],
    loading: true,
    error: null,
    pagination: null,
    searchParams: { ...DEFAULT_PAGINATION },
  });

  const [modal, setModal] = useState<ModalState>({
    isOpen: false,
    mode: null,
    user: null,
  });

  const [searchTerm, setSearchTerm] = useState('');
  const [filterUserType, setFilterUserType] = useState<UserType | ''>('');
  const [selectedUsers, setSelectedUsers] = useState<string[]>([]);
  const [bulkLoading, setBulkLoading] = useState(false);
  const [message, setMessage] = useState<{ type: 'success' | 'error'; text: string } | null>(null);

  // Refs for keyboard navigation
  const searchInputRef = useRef<HTMLInputElement>(null);
  const filterSelectRef = useRef<HTMLSelectElement>(null);
  const addButtonRef = useRef<HTMLButtonElement>(null);

  // Breadcrumb navigation
  const breadcrumbs: BreadcrumbItem[] = [
    { label: 'Home', href: '/' },
    { label: 'User Management' },
  ];

  // Fetch users data
  const fetchUsers = useCallback(async (params?: Partial<UserSearchRequest>) => {
    try {
      setState(prev => ({ ...prev, loading: true, error: null }));
      
      const searchParams = {
        ...state.searchParams,
        ...params,
      };

      const response = await userService.listUsers(searchParams);
      console.log('Fetched users:', response);
      setState(prev => ({
        ...prev,
        users: response.users,
        pagination: response.pagination,
        searchParams,
        loading: false,
      }));
    } catch (error) {
      const errorMessage = error instanceof Error ? error.message : 'Failed to fetch users';
      setState(prev => ({
        ...prev,
        error: errorMessage,
        loading: false,
      }));
    }
  }, [state.searchParams]);

  // Search functionality
  const handleSearch = useCallback(async () => {
    const params: UserSearchRequest = {
      ...DEFAULT_PAGINATION,
      page: 0,
    };

    if (searchTerm.trim()) {
      params.searchTerm = searchTerm.trim();
    }

    if (filterUserType) {
      params.userType = filterUserType;
    }

    await fetchUsers(params);
  }, [searchTerm, filterUserType, fetchUsers]);

  // Clear search and filters
  const handleClearSearch = useCallback(async () => {
    setSearchTerm('');
    setFilterUserType('');
    await fetchUsers({ ...DEFAULT_PAGINATION, page: 0 });
  }, [fetchUsers]);

  // Pagination handlers
  const handlePageChange = useCallback(async (page: number) => {
    await fetchUsers({ page });
  }, [fetchUsers]);

  // Modal handlers
  const openModal = useCallback((mode: 'create' | 'edit' | 'view' | 'delete', user?: UserResponse) => {
    setModal({
      isOpen: true,
      mode,
      user: user || null,
    });
  }, []);

  const closeModal = useCallback(() => {
    setModal({
      isOpen: false,
      mode: null,
      user: null,
    });
  }, []);

  // User operations
  const handleCreateUser = useCallback(async (userData: CreateUserRequest) => {
    try {
      await userService.createUser(userData);
      setMessage({ type: 'success', text: 'User created successfully' });
      closeModal();
      await fetchUsers();
    } catch (error) {
      const errorMessage = error instanceof Error ? error.message : 'Failed to create user';
      setMessage({ type: 'error', text: errorMessage });
      throw error;
    }
  }, [fetchUsers, closeModal]);

  const handleUpdateUser = useCallback(async (userData: UpdateUserRequest) => {
    if (!modal.user) return;

    try {
      await userService.updateUser(modal.user.userId, userData);
      setMessage({ type: 'success', text: 'User updated successfully' });
      closeModal();
      await fetchUsers();
    } catch (error) {
      const errorMessage = error instanceof Error ? error.message : 'Failed to update user';
      setMessage({ type: 'error', text: errorMessage });
      throw error;
    }
  }, [modal.user, fetchUsers, closeModal]);

  const handleDeleteUser = useCallback(async (userId: string) => {
    try {
      await userService.deleteUser(userId);
      setMessage({ type: 'success', text: 'User deleted successfully' });
      closeModal();
      await fetchUsers();
    } catch (error) {
      const errorMessage = error instanceof Error ? error.message : 'Failed to delete user';
      setMessage({ type: 'error', text: errorMessage });
    }
  }, [fetchUsers, closeModal]);

  // Bulk operations
  const handleSelectUser = useCallback((userId: string, selected: boolean) => {
    setSelectedUsers(prev => 
      selected 
        ? [...prev, userId]
        : prev.filter(id => id !== userId)
    );
  }, []);

  const handleSelectAll = useCallback((selected: boolean) => {
    setSelectedUsers(selected ? state.users.map(user => user.userId) : []);
  }, [state.users]);

  const handleBulkDelete = useCallback(async () => {
    if (selectedUsers.length === 0) return;

    try {
      setBulkLoading(true);
      const result = await userService.bulkDeleteUsers(selectedUsers);
      
      if (result.deleted.length > 0) {
        setMessage({ 
          type: 'success', 
          text: `${result.deleted.length} user(s) deleted successfully` 
        });
      }
      
      if (result.failed.length > 0) {
        setMessage({ 
          type: 'error', 
          text: `Failed to delete ${result.failed.length} user(s)` 
        });
      }

      setSelectedUsers([]);
      await fetchUsers();
    } catch (error) {
      const errorMessage = error instanceof Error ? error.message : 'Failed to delete users';
      setMessage({ type: 'error', text: errorMessage });
    } finally {
      setBulkLoading(false);
    }
  }, [selectedUsers, fetchUsers]);

  // Keyboard navigation
  const handleKeyDown = useCallback((event: KeyboardEvent) => {
    switch (event.key) {
      case 'F1':
        event.preventDefault();
        openModal('create');
        break;
      case 'F2':
        event.preventDefault();
        handleSearch();
        break;
      case 'F3':
        event.preventDefault();
        handleClearSearch();
        break;
      case 'F5':
        event.preventDefault();
        fetchUsers();
        break;
      case 'Escape':
        if (modal.isOpen) {
          event.preventDefault();
          closeModal();
        }
        break;
    }
  }, [openModal, handleSearch, handleClearSearch, fetchUsers, modal.isOpen, closeModal]);

  // Effects
  useEffect(() => {
    fetchUsers();
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);

  useEffect(() => {
    window.addEventListener('keydown', handleKeyDown);
    return () => {
      window.removeEventListener('keydown', handleKeyDown);
    };
  }, [handleKeyDown]);

  useEffect(() => {
    if (message) {
      const timer = setTimeout(() => {
        setMessage(null);
      }, 5000);
      return () => clearTimeout(timer);
    }
  }, [message]);

  // User type options for filter dropdown
  const userTypeOptions = [
    { value: '', label: 'All Types' },
    ...Object.values(UserType).map(type => ({
      value: type,
      label: USER_TYPE_LABELS[type],
    })),
  ];

  return (
    <div className="min-h-screen bg-gray-50">
      {/* Header */}
      <div className="bg-white border-b">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
          <div className="py-4">
            {/* Breadcrumbs */}
            <nav className="flex mb-4" aria-label="Breadcrumb">
              <ol className="flex items-center space-x-2">
                {breadcrumbs.map((item, index) => (
                  <li key={index} className="flex items-center">
                    {index > 0 && (
                      <span className="mx-2 text-gray-400">/</span>
                    )}
                    {item.href ? (
                      <Link
                        href={item.href}
                        className="text-blue-600 hover:text-blue-800 text-sm font-medium"
                      >
                        {item.label}
                      </Link>
                    ) : (
                      <span className="text-gray-900 text-sm font-medium">
                        {item.label}
                      </span>
                    )}
                  </li>
                ))}
              </ol>
            </nav>

            {/* Page Title */}
            <div className="flex justify-between items-center">
              <div>
                <h1 className="text-3xl font-bold text-gray-900">
                  User Management (COUSR00C)
                </h1>
                <p className="text-gray-600 mt-1">
                  Manage user accounts and permissions
                </p>
              </div>
              <Button
                ref={addButtonRef}
                onClick={() => openModal('create')}
                className="min-w-[140px]"
              >
                Add User (F1)
              </Button>
            </div>
          </div>
        </div>
      </div>

      {/* Main Content */}
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        {/* Success/Error Messages */}
        {message && (
          <div className={`mb-6 p-4 rounded-md ${
            message.type === 'success' 
              ? 'bg-green-50 border border-green-200 text-green-800' 
              : 'bg-red-50 border border-red-200 text-red-800'
          }`}>
            <p className="font-medium">{message.text}</p>
          </div>
        )}

        {/* Search and Filters */}
        <div className="bg-white rounded-lg shadow-sm border p-6 mb-6">
          <h2 className="text-lg font-semibold text-gray-900 mb-4">
            Search & Filter
          </h2>
          
          <div className="grid grid-cols-1 md:grid-cols-3 gap-4 mb-4">
            <Input
              ref={searchInputRef}
              label="Search Users"
              value={searchTerm}
              onChange={(e) => setSearchTerm(e.target.value)}
              placeholder="Search by User ID, Name..."
              onKeyDown={(e) => {
                if (e.key === 'Enter') {
                  handleSearch();
                }
              }}
            />
            
            <Select
              ref={filterSelectRef}
              label="User Type"
              value={filterUserType}
              onChange={(e) => setFilterUserType(e.target.value as UserType | '')}
              options={userTypeOptions}
            />
            
            <div className="flex items-end gap-2">
              <Button
                onClick={handleSearch}
                className="min-w-[100px]"
              >
                Search (F2)
              </Button>
              <Button
                onClick={handleClearSearch}
                variant="outline"
                className="min-w-[100px]"
              >
                Clear (F3)
              </Button>
            </div>
          </div>

          {/* Bulk Actions */}
          {selectedUsers.length > 0 && (
            <div className="flex items-center gap-4 p-4 bg-blue-50 rounded-md">
              <span className="text-sm font-medium text-blue-900">
                {selectedUsers.length} user(s) selected
              </span>
              <Button
                onClick={handleBulkDelete}
                disabled={bulkLoading}
                variant="destructive"
                size="sm"
              >
                {bulkLoading ? 'Deleting...' : 'Delete Selected'}
              </Button>
              <Button
                onClick={() => setSelectedUsers([])}
                variant="outline"
                size="sm"
              >
                Clear Selection
              </Button>
            </div>
          )}
        </div>

        {/* Users Table */}
        <div className="bg-white rounded-lg shadow-sm border">
          <div className="p-6 border-b">
            <div className="flex justify-between items-center">
              <h2 className="text-lg font-semibold text-gray-900">
                Users List
                {state.pagination && (
                  <span className="text-sm font-normal text-gray-500 ml-2">
                    ({state.pagination.totalItems} total)
                  </span>
                )}
              </h2>
              <Button
                onClick={() => fetchUsers()}
                disabled={state.loading}
                variant="outline"
                size="sm"
              >
                {state.loading ? 'Refreshing...' : 'Refresh (F5)'}
              </Button>
            </div>
          </div>

          {state.error ? (
            <div className="p-6">
              <div className="text-center py-8">
                <p className="text-red-600 font-medium mb-4">
                  Error: {state.error}
                </p>
                <Button onClick={() => fetchUsers()} variant="outline">
                  Try Again
                </Button>
              </div>
            </div>
          ) : (
            <div className="overflow-x-auto">
              <Table>
                <TableHeader>
                  <TableRow>
                    {selectedUsers.length >= 0 && (
                      <TableHead className="w-12">
                        <input
                          type="checkbox"
                          checked={selectedUsers.length === state.users.length && state.users.length > 0}
                          onChange={(e) => handleSelectAll(e.target.checked)}
                          className="rounded border-gray-300"
                        />
                      </TableHead>
                    )}
                    <TableHead>User ID</TableHead>
                    <TableHead>First Name</TableHead>
                    <TableHead>Last Name</TableHead>
                    <TableHead>User Type</TableHead>
                    <TableHead>Created</TableHead>
                    <TableHead>Actions</TableHead>
                  </TableRow>
                </TableHeader>
                <TableBody>
                  {state.loading ? (
                    <TableRow>
                      <TableCell colSpan={7} className="text-center py-8">
                        <div className="flex justify-center items-center">
                          <div className="animate-spin rounded-full h-8 w-8 border-b-2 border-blue-600"></div>
                          <span className="ml-3">Loading users...</span>
                        </div>
                      </TableCell>
                    </TableRow>
                  ) : state.users.length === 0 ? (
                    <TableRow>
                      <TableCell colSpan={7} className="text-center py-8 text-gray-500">
                        No users found. Click &apos;Add User&apos; to create the first user.
                      </TableCell>
                    </TableRow>
                  ) : (
                    state.users.map((user) => (
                      <TableRow key={user.userId}>
                        <TableCell>
                          <input
                            type="checkbox"
                            checked={selectedUsers.includes(user.userId)}
                            onChange={(e) => handleSelectUser(user.userId, e.target.checked)}
                            className="rounded border-gray-300"
                          />
                        </TableCell>
                        <TableCell className="font-mono">{user.userId}</TableCell>
                        <TableCell>{user.firstName}</TableCell>
                        <TableCell>{user.lastName}</TableCell>
                        <TableCell>{USER_TYPE_LABELS[user.userType as UserType] || user.userType}</TableCell>
                        <TableCell>{new Date(user.createdAt).toLocaleDateString()}</TableCell>
                        <TableCell>
                          <div className="flex gap-2">
                            <Button
                              size="sm"
                              variant="outline"
                              onClick={() => openModal('view', user)}
                            >
                              View
                            </Button>
                            <Button
                              size="sm"
                              variant="secondary"
                              onClick={() => openModal('edit', user)}
                            >
                              Edit
                            </Button>
                            <Button
                              size="sm"
                              variant="destructive"
                              onClick={() => openModal('delete', user)}
                            >
                              Delete
                            </Button>
                          </div>
                        </TableCell>
                      </TableRow>
                    ))
                  )}
                </TableBody>
              </Table>

              {/* Pagination */}
              {state.pagination && state.pagination.totalPages > 1 && (
                <div className="flex items-center justify-between px-6 py-4 border-t">
                  <div className="text-sm text-gray-700">
                    Showing page {state.pagination.currentPage} of {state.pagination.totalPages}
                    {' '}({state.pagination.totalItems} total users)
                  </div>
                  <div className="flex gap-2">
                    <Button
                      size="sm"
                      variant="outline"
                      disabled={!state.pagination.hasPreviousPage}
                      onClick={() => handlePageChange(state.pagination!.currentPage - 1)}
                    >
                      Previous
                    </Button>
                    <Button
                      size="sm"
                      variant="outline"
                      disabled={!state.pagination.hasNextPage}
                      onClick={() => handlePageChange(state.pagination!.currentPage + 1)}
                    >
                      Next
                    </Button>
                  </div>
                </div>
              )}
            </div>
          )}
        </div>

        {/* Keyboard Navigation Help */}
        <div className="mt-8 bg-white rounded-lg shadow-sm border p-6">
          <h3 className="text-sm font-medium text-gray-900 mb-3">
            Keyboard Shortcuts:
          </h3>
          <div className="grid grid-cols-2 md:grid-cols-4 gap-3 text-xs text-gray-600">
            <div>F1: Add User</div>
            <div>F2: Search</div>
            <div>F3: Clear Search</div>
            <div>F5: Refresh</div>
            <div>Esc: Close Modal</div>
            <div>Tab: Navigate Fields</div>
            <div>Enter: Confirm Action</div>
            <div>↑/↓: Navigate Table</div>
          </div>
        </div>
      </div>

      {/* Modals */}
      {modal.isOpen && modal.mode === 'create' && (
        <Modal
          isOpen={true}
          onClose={closeModal}
          title="Add New User"
        >
          <UserForm
            onSave={handleCreateUser}
            onSaveAndExit={handleCreateUser}
            onCancel={closeModal}
          />
        </Modal>
      )}

      {modal.isOpen && modal.mode === 'edit' && modal.user && (
        <Modal
          isOpen={true}
          onClose={closeModal}
          title={`Edit User: ${modal.user.userId}`}
        >
          <div className="p-6">
            <p className="text-sm text-gray-600 mb-4">
              Editing user: {modal.user.firstName} {modal.user.lastName}
            </p>
            <UserForm
              onSave={async (userData) => {
                await handleUpdateUser({
                  firstName: userData.firstName,
                  lastName: userData.lastName,
                  userType: userData.userType,
                  ...(userData.password && { password: userData.password }),
                });
              }}
              onCancel={closeModal}
            />
          </div>
        </Modal>
      )}

      {modal.isOpen && modal.mode === 'view' && modal.user && (
        <Modal
          isOpen={true}
          onClose={closeModal}
          title={`View User: ${modal.user.userId}`}
        >
          <div className="p-6">
            <div className="space-y-4">
              <div>
                <label className="block text-sm font-medium text-gray-700">User ID</label>
                <p className="mt-1 text-sm text-gray-900 font-mono">{modal.user.userId}</p>
              </div>
              <div>
                <label className="block text-sm font-medium text-gray-700">First Name</label>
                <p className="mt-1 text-sm text-gray-900">{modal.user.firstName}</p>
              </div>
              <div>
                <label className="block text-sm font-medium text-gray-700">Last Name</label>
                <p className="mt-1 text-sm text-gray-900">{modal.user.lastName}</p>
              </div>
              <div>
                <label className="block text-sm font-medium text-gray-700">User Type</label>
                <p className="mt-1 text-sm text-gray-900">
                  {USER_TYPE_LABELS[modal.user.userType as UserType] || modal.user.userType}
                </p>
              </div>
              <div>
                <label className="block text-sm font-medium text-gray-700">Created</label>
                <p className="mt-1 text-sm text-gray-900">
                  {new Date(modal.user.createdAt).toLocaleString()}
                </p>
              </div>
              <div>
                <label className="block text-sm font-medium text-gray-700">Last Updated</label>
                <p className="mt-1 text-sm text-gray-900">
                  {new Date(modal.user.updatedAt).toLocaleString()}
                </p>
              </div>
            </div>
            <div className="mt-6 flex justify-end gap-3">
              <Button onClick={closeModal} variant="outline">
                Close
              </Button>
              <Button onClick={() => modal.user && openModal('edit', modal.user)}>
                Edit User
              </Button>
            </div>
          </div>
        </Modal>
      )}

      {modal.isOpen && modal.mode === 'delete' && modal.user && (
        <Modal
          isOpen={true}
          onClose={closeModal}
          title="Confirm Delete"
        >
          <div className="p-6">
            <p className="text-sm text-gray-900 mb-4">
              Are you sure you want to delete user <strong>{modal.user.userId}</strong>?
            </p>
            <p className="text-sm text-gray-600 mb-6">
              User: {modal.user.firstName} {modal.user.lastName}
              <br />
              This action cannot be undone.
            </p>
            <div className="flex justify-end gap-3">
              <Button onClick={closeModal} variant="outline">
                Cancel
              </Button>
              <Button
                onClick={() => handleDeleteUser(modal.user!.userId)}
                variant="destructive"
              >
                Delete User
              </Button>
            </div>
          </div>
        </Modal>
      )}
    </div>
  );
}