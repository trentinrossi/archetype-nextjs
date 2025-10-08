'use client';

import { useState, useEffect, useCallback } from 'react';
import { useRouter, useSearchParams } from 'next/navigation';
import { Button, Input, Table } from '@/components/ui';
import userSecurityService from '@/services/userSecurityService';
import {
  UserSecurity,
  GetUsersRequest,
  InvalidKeyRequest
} from '@/types/userSecurity';

interface UserListState {
  isLoading: boolean;
  isSearching: boolean;
  error: string;
  message: string;
  currentDateTime: string;
  users: UserSecurity[];
  currentPage: number;
  totalPages: number;
  totalUsers: number;
  pageSize: number;
  hasNext: boolean;
  hasPrev: boolean;
  searchUserId: string;
  selectedActions: { [key: string]: 'U' | 'D' | '' };
}

export default function UserListPage() {
  const router = useRouter();
  const searchParams = useSearchParams();
  const initialPage = parseInt(searchParams.get('page') || '1', 10);
  const initialSearch = searchParams.get('search') || '';

  const [state, setState] = useState<UserListState>({
    isLoading: false,
    isSearching: false,
    error: '',
    message: '',
    currentDateTime: '',
    users: [],
    currentPage: initialPage,
    totalPages: 0,
    totalUsers: 0,
    pageSize: 10,
    hasNext: false,
    hasPrev: false,
    searchUserId: initialSearch,
    selectedActions: {}
  });

  // Update current date and time
  const updateDateTime = useCallback((): void => {
    const now = new Date();
    const dateTimeString = now.toLocaleString('en-US', {
      year: 'numeric',
      month: '2-digit',
      day: '2-digit',
      hour: '2-digit',
      minute: '2-digit',
      second: '2-digit',
      hour12: false
    });
    setState(prev => ({ ...prev, currentDateTime: dateTimeString }));
  }, []);

  // Load users data
  const loadUsers = useCallback(async (page: number = 1, search: string = ''): Promise<void> => {
    setState(prev => ({ 
      ...prev, 
      isLoading: true, 
      error: '', 
      message: search ? `Searching for users starting with "${search}"...` : 'Loading users...' 
    }));

    try {
      const params: GetUsersRequest = {
        page,
        limit: state.pageSize,
        sortBy: 'userId',
        sortOrder: 'asc'
      };

      if (search.trim()) {
        params.search = search.trim();
      }

      const response = await userSecurityService.getUsers(params);

      if (response.success && response.data) {
        const { users, pagination } = response.data;
        
        setState(prev => ({
          ...prev,
          isLoading: false,
          users,
          currentPage: pagination.page,
          totalPages: pagination.totalPages,
          totalUsers: pagination.total,
          hasNext: pagination.hasNext,
          hasPrev: pagination.hasPrev,
          selectedActions: {},
          message: search 
            ? `Found ${pagination.total} users starting with "${search}". Page ${pagination.page} of ${pagination.totalPages}.`
            : `Displaying ${users.length} of ${pagination.total} users. Page ${pagination.page} of ${pagination.totalPages}.`
        }));
      } else {
        setState(prev => ({
          ...prev,
          isLoading: false,
          users: [],
          currentPage: 1,
          totalPages: 0,
          totalUsers: 0,
          hasNext: false,
          hasPrev: false,
          selectedActions: {},
          error: response.error || 'Failed to load users'
        }));
      }
    } catch (error) {
      console.error('Load users error:', error);
      setState(prev => ({
        ...prev,
        isLoading: false,
        users: [],
        currentPage: 1,
        totalPages: 0,
        totalUsers: 0,
        hasNext: false,
        hasPrev: false,
        selectedActions: {},
        error: error instanceof Error ? error.message : 'Failed to load users'
      }));
    }
  }, [state.pageSize]);

  // Initialize page
  useEffect(() => {
    updateDateTime();
    
    // Update time every second
    const interval = setInterval(updateDateTime, 1000);
    
    // Load initial data
    loadUsers(initialPage, initialSearch);

    return () => clearInterval(interval);
  }, [updateDateTime, loadUsers, initialPage, initialSearch]);

  // Handle search by User ID
  const handleSearch = useCallback(async (): Promise<void> => {
    const searchTerm = state.searchUserId.trim();
    
    setState(prev => ({ ...prev, isSearching: true }));
    
    // Update URL with search parameter
    const params = new URLSearchParams();
    if (searchTerm) {
      params.set('search', searchTerm);
    }
    params.set('page', '1');
    
    router.push(`/users?${params.toString()}`, { scroll: false });
    
    await loadUsers(1, searchTerm);
    
    setState(prev => ({ ...prev, isSearching: false }));
  }, [state.searchUserId, router, loadUsers]);

  // Handle previous page (PF7)
  const handlePreviousPage = useCallback(async (): Promise<void> => {
    if (!state.hasPrev || state.isLoading) {
      setState(prev => ({ ...prev, message: 'Already at first page.' }));
      return;
    }

    const newPage = state.currentPage - 1;
    
    // Update URL
    const params = new URLSearchParams();
    if (state.searchUserId.trim()) {
      params.set('search', state.searchUserId.trim());
    }
    params.set('page', newPage.toString());
    
    router.push(`/users?${params.toString()}`, { scroll: false });
    
    await loadUsers(newPage, state.searchUserId);
  }, [state.hasPrev, state.isLoading, state.currentPage, state.searchUserId, router, loadUsers]);

  // Handle next page (PF8)
  const handleNextPage = useCallback(async (): Promise<void> => {
    if (!state.hasNext || state.isLoading) {
      setState(prev => ({ ...prev, message: 'Already at last page.' }));
      return;
    }

    const newPage = state.currentPage + 1;
    
    // Update URL
    const params = new URLSearchParams();
    if (state.searchUserId.trim()) {
      params.set('search', state.searchUserId.trim());
    }
    params.set('page', newPage.toString());
    
    router.push(`/users?${params.toString()}`, { scroll: false });
    
    await loadUsers(newPage, state.searchUserId);
  }, [state.hasNext, state.isLoading, state.currentPage, state.searchUserId, router, loadUsers]);

  // Handle user action selection
  const handleActionSelect = useCallback((userId: string, action: 'U' | 'D' | ''): void => {
    setState(prev => ({
      ...prev,
      selectedActions: {
        ...prev.selectedActions,
        [userId]: action
      }
    }));
  }, []);

  // Process selected actions
  const processSelectedActions = useCallback((): void => {
    const actionsToProcess = Object.entries(state.selectedActions).filter(([_, action]) => action !== '');
    
    if (actionsToProcess.length === 0) {
      setState(prev => ({ ...prev, message: 'No users selected for action.' }));
      return;
    }

    // Process each selected action
    actionsToProcess.forEach(([userId, action]) => {
      const user = state.users.find(u => u.userId === userId);
      if (user) {
        if (action === 'U') {
          // Navigate to update user page
          router.push(`/users/update?userId=${userId}`);
        } else if (action === 'D') {
          // Navigate to delete user page
          router.push(`/users/delete?userId=${userId}`);
        }
      }
    });
  }, [state.selectedActions, state.users, router]);

  // Handle exit (PF3)
  const handleExit = useCallback((): void => {
    router.push('/admin');
  }, [router]);

  // Handle invalid key press
  const handleInvalidKey = useCallback(async (key: string): Promise<void> => {
    try {
      const invalidKeyRequest: InvalidKeyRequest = {
        attemptedKey: key,
        timestamp: new Date().toISOString()
      };

      const response = await userSecurityService.handleInvalidKey(invalidKeyRequest);
      
      if (response.success && response.data?.message) {
        setState(prev => ({ ...prev, error: response.data!.message }));
      } else {
        setState(prev => ({ 
          ...prev, 
          error: `Invalid key pressed: ${key}. Use Enter (Search), PF3 (Exit), PF7 (Previous), PF8 (Next).` 
        }));
      }
    } catch (error) {
      console.error('Invalid key handling error:', error);
      setState(prev => ({ 
        ...prev, 
        error: `Invalid key pressed: ${key}. Use Enter (Search), PF3 (Exit), PF7 (Previous), PF8 (Next).` 
      }));
    }
  }, []);

  // Handle keyboard events
  const handleKeyDown = useCallback(async (event: React.KeyboardEvent): Promise<void> => {
    if (state.isLoading || state.isSearching) {
      return;
    }

    // Clear previous messages for function keys
    if (event.key.startsWith('F') || event.key === 'Enter') {
      event.preventDefault();
      setState(prev => ({ ...prev, error: '', message: '' }));
    }

    switch (event.key) {
      case 'Enter':
        await handleSearch();
        break;
      
      case 'F3':
        handleExit();
        break;
      
      case 'F7':
        await handlePreviousPage();
        break;
      
      case 'F8':
        await handleNextPage();
        break;
      
      case 'F1':
      case 'F2':
      case 'F4':
      case 'F5':
      case 'F6':
      case 'F9':
      case 'F10':
      case 'F11':
      case 'F12':
      case 'Escape':
        await handleInvalidKey(event.key);
        break;
      
      default:
        // Handle other special key combinations as invalid
        if (event.ctrlKey || event.altKey || event.metaKey) {
          await handleInvalidKey(`${event.ctrlKey ? 'Ctrl+' : ''}${event.altKey ? 'Alt+' : ''}${event.metaKey ? 'Meta+' : ''}${event.key}`);
        }
        break;
    }
  }, [state.isLoading, state.isSearching, handleSearch, handleExit, handlePreviousPage, handleNextPage, handleInvalidKey]);

  // Table columns configuration
  const columns = [
    {
      key: 'action' as const,
      label: 'Action',
      width: '80px',
      align: 'center' as const
    },
    {
      key: 'userId' as const,
      label: 'User ID',
      width: '120px',
      align: 'left' as const
    },
    {
      key: 'firstName' as const,
      label: 'First Name',
      width: '150px',
      align: 'left' as const
    },
    {
      key: 'lastName' as const,
      label: 'Last Name',
      width: '150px',
      align: 'left' as const
    },
    {
      key: 'userType' as const,
      label: 'User Type',
      width: '120px',
      align: 'left' as const
    }
  ];

  // Render table row
  const renderRow = (user: UserSecurity, index: number) => (
    <tr key={user.userId} className={index % 2 === 0 ? 'bg-white' : 'bg-gray-50'}>
      <td className="px-3 py-2 text-center">
        <select
          value={state.selectedActions[user.userId] || ''}
          onChange={(e) => handleActionSelect(user.userId, e.target.value as 'U' | 'D' | '')}
          className="text-sm border border-gray-300 rounded px-1 py-0.5 w-12"
        >
          <option value="">-</option>
          <option value="U">U</option>
          <option value="D">D</option>
        </select>
      </td>
      <td className="px-3 py-2 text-sm font-mono text-gray-900">{user.userId}</td>
      <td className="px-3 py-2 text-sm text-gray-900">{user.firstName}</td>
      <td className="px-3 py-2 text-sm text-gray-900">{user.lastName}</td>
      <td className="px-3 py-2 text-sm text-gray-900">{user.roles.join(', ')}</td>
    </tr>
  );

  return (
    <div 
      className="min-h-screen bg-gray-50 py-8 px-4 sm:px-6 lg:px-8"
      onKeyDown={handleKeyDown}
      tabIndex={-1}
    >
      <div className="max-w-6xl mx-auto">
        {/* Header */}
        <div className="bg-white shadow rounded-lg p-6 mb-8">
          <div className="flex justify-between items-center">
            <div>
              <h1 className="text-3xl font-bold text-gray-900">User List Management</h1>
              <p className="mt-1 text-sm text-gray-600">Transaction ID: COUSR00C</p>
            </div>
            <div className="text-right">
              <p className="text-sm text-gray-600">Current Date/Time:</p>
              <p className="text-lg font-mono text-gray-900">{state.currentDateTime}</p>
            </div>
          </div>
        </div>

        {/* Messages */}
        {state.error && (
          <div className="mb-6 rounded-md bg-red-50 p-4">
            <div className="flex">
              <div className="flex-shrink-0">
                <svg className="h-5 w-5 text-red-400" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 20 20" fill="currentColor">
                  <path fillRule="evenodd" d="M10 18a8 8 0 100-16 8 8 0 000 16zM8.707 7.293a1 1 0 00-1.414 1.414L8.586 10l-1.293 1.293a1 1 0 101.414 1.414L10 11.414l1.293 1.293a1 1 0 001.414-1.414L11.414 10l1.293-1.293a1 1 0 00-1.414-1.414L10 8.586 8.707 7.293z" clipRule="evenodd" />
                </svg>
              </div>
              <div className="ml-3">
                <p className="text-sm text-red-800">{state.error}</p>
              </div>
            </div>
          </div>
        )}

        {state.message && !state.error && (
          <div className="mb-6 rounded-md bg-blue-50 p-4">
            <div className="flex">
              <div className="flex-shrink-0">
                <svg className="h-5 w-5 text-blue-400" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 20 20" fill="currentColor">
                  <path fillRule="evenodd" d="M18 10a8 8 0 11-16 0 8 8 0 0116 0zm-7-4a1 1 0 11-2 0 1 1 0 012 0zM9 9a1 1 0 000 2v3a1 1 0 001 1h1a1 1 0 100-2v-3a1 1 0 00-1-1H9z" clipRule="evenodd" />
                </svg>
              </div>
              <div className="ml-3">
                <p className="text-sm text-blue-800">{state.message}</p>
              </div>
            </div>
          </div>
        )}

        {/* Search Section */}
        <div className="bg-white shadow rounded-lg p-6 mb-8">
          <h2 className="text-xl font-semibold text-gray-900 mb-4">Filter Users</h2>
          
          <div className="flex items-end space-x-4">
            <div className="flex-1">
              <label htmlFor="searchUserId" className="block text-sm font-medium text-gray-700 mb-2">
                Starting User ID (optional)
              </label>
              <Input
                id="searchUserId"
                type="text"
                value={state.searchUserId}
                onChange={(e) => setState(prev => ({ ...prev, searchUserId: e.target.value }))}
                placeholder="Enter starting User ID to filter"
                disabled={state.isLoading || state.isSearching}
                className="w-full"
              />
            </div>
            <Button
              onClick={handleSearch}
              disabled={state.isLoading || state.isSearching}
              className="px-6 py-2"
            >
              {state.isSearching ? 'Searching...' : 'Search (Enter)'}
            </Button>
          </div>
        </div>

        {/* Users Table */}
        <div className="bg-white shadow rounded-lg overflow-hidden mb-8">
          <div className="px-6 py-4 border-b border-gray-200">
            <div className="flex justify-between items-center">
              <h2 className="text-xl font-semibold text-gray-900">Users</h2>
              <div className="text-sm text-gray-600">
                Page {state.currentPage} of {state.totalPages} ({state.totalUsers} total users)
              </div>
            </div>
          </div>

          {state.isLoading ? (
            <div className="p-8 text-center">
              <div className="inline-flex items-center">
                <svg className="animate-spin -ml-1 mr-3 h-5 w-5 text-gray-500" xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24">
                  <circle className="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" strokeWidth="4"></circle>
                  <path className="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"></path>
                </svg>
                Loading users...
              </div>
            </div>
          ) : state.users.length === 0 ? (
            <div className="p-8 text-center text-gray-500">
              {state.searchUserId ? 'No users found matching your search criteria.' : 'No users found.'}
            </div>
          ) : (
            <div className="overflow-x-auto">
              <table className="min-w-full divide-y divide-gray-200">
                <thead className="bg-gray-50">
                  <tr>
                    {columns.map((column) => (
                      <th
                        key={column.key}
                        className={`px-3 py-3 text-xs font-medium text-gray-500 uppercase tracking-wider ${
                          column.align === 'center' ? 'text-center' : 
                          column.align === 'right' ? 'text-right' : 'text-left'
                        }`}
                        style={{ width: column.width }}
                      >
                        {column.label}
                      </th>
                    ))}
                  </tr>
                </thead>
                <tbody className="bg-white divide-y divide-gray-200">
                  {state.users.map((user, index) => renderRow(user, index))}
                </tbody>
              </table>
            </div>
          )}
        </div>

        {/* Pagination and Actions */}
        <div className="flex justify-between items-center mb-8">
          <div className="flex space-x-4">
            <Button
              onClick={handlePreviousPage}
              disabled={!state.hasPrev || state.isLoading}
              variant="secondary"
              className="px-6 py-2"
            >
              Previous (F7)
            </Button>

            <Button
              onClick={handleNextPage}
              disabled={!state.hasNext || state.isLoading}
              variant="secondary"
              className="px-6 py-2"
            >
              Next (F8)
            </Button>
          </div>

          <div className="flex space-x-4">
            <Button
              onClick={processSelectedActions}
              disabled={state.isLoading || Object.values(state.selectedActions).every(action => action === '')}
              variant="primary"
              className="px-6 py-2"
            >
              Process Selected
            </Button>

            <Button
              onClick={handleExit}
              disabled={state.isLoading}
              variant="secondary"
              className="px-6 py-2"
            >
              Exit (F3)
            </Button>
          </div>
        </div>

        {/* Instructions */}
        <div className="bg-gray-50 rounded-lg p-6">
          <h3 className="text-lg font-medium text-gray-900 mb-4">Program Instructions</h3>
          
          <div className="grid gap-6 md:grid-cols-2">
            <div>
              <h4 className="text-sm font-medium text-gray-900 mb-2">User Selection:</h4>
              <ul className="text-sm text-gray-600 space-y-1">
                <li>• Select 'U' in Action column to update user</li>
                <li>• Select 'D' in Action column to delete user</li>
                <li>• Click "Process Selected" to execute actions</li>
                <li>• Multiple users can be selected at once</li>
              </ul>
            </div>
            
            <div>
              <h4 className="text-sm font-medium text-gray-900 mb-2">Navigation:</h4>
              <ul className="text-sm text-gray-600 space-y-1">
                <li>• <kbd className="px-1 py-0.5 text-xs font-mono bg-gray-200 rounded">F7</kbd> - Previous page (10 users)</li>
                <li>• <kbd className="px-1 py-0.5 text-xs font-mono bg-gray-200 rounded">F8</kbd> - Next page (10 users)</li>
                <li>• <kbd className="px-1 py-0.5 text-xs font-mono bg-gray-200 rounded">Enter</kbd> - Search/Filter users</li>
                <li>• <kbd className="px-1 py-0.5 text-xs font-mono bg-gray-200 rounded">F3</kbd> - Exit to Admin Menu</li>
              </ul>
            </div>
          </div>

          <div className="mt-4">
            <h4 className="text-sm font-medium text-gray-900 mb-2">Search/Filter:</h4>
            <ul className="text-sm text-gray-600 space-y-1">
              <li>• Enter starting User ID to filter users</li>
              <li>• Leave blank to show all users</li>
              <li>• Press <kbd className="px-1 py-0.5 text-xs font-mono bg-gray-200 rounded">Enter</kbd> to apply filter</li>
              <li>• Results are paginated (10 users per page)</li>
            </ul>
          </div>

          <div className="mt-4 p-3 bg-blue-50 rounded-md border border-blue-200">
            <p className="text-sm text-blue-800">
              <strong>Note:</strong> This program displays users from the USRSEC file in paginated format. 
              Use the Action column to select users for update or deletion operations.
            </p>
          </div>
        </div>
      </div>
    </div>
  );
}