'use client';

import React, { useState, useEffect, useCallback } from 'react';
import { useRouter } from 'next/navigation';
import { Button, Input, Table } from '@/components/ui';
import { userService } from '@/services/userService';
import {
  User,
  UserListResponse,
  Pageable,
  UserActionSelection
} from '@/types/user';

interface UserListPageState {
  users: User[];
  pagination: UserListResponse | null;
  isLoading: boolean;
  error: string | null;
  searchUserId: string;
  selectedActions: { [key: string]: string }; // userId -> action
  message: string;
  messageType: 'success' | 'error' | 'info' | '';
  currentPage: number;
}

const INITIAL_STATE: UserListPageState = {
  users: [],
  pagination: null,
  isLoading: false,
  error: null,
  searchUserId: '',
  selectedActions: {},
  message: '',
  messageType: '',
  currentPage: 0
};

const PAGE_SIZE = 10;

export default function UserListPage() {
  const router = useRouter();
  const [state, setState] = useState<UserListPageState>(INITIAL_STATE);

  const updateState = useCallback((updates: Partial<UserListPageState>) => {
    setState(prev => ({ ...prev, ...updates }));
  }, []);

  const setMessage = useCallback((message: string, type: 'success' | 'error' | 'info' = 'info') => {
    updateState({ message, messageType: type });
    if (message) {
      setTimeout(() => {
        updateState({ message: '', messageType: '' });
      }, 5000);
    }
  }, [updateState]);

  const fetchUsers = useCallback(async (page: number = 0, searchId?: string) => {
    updateState({ isLoading: true, error: null });

    try {
      const params: Pageable = {
        page,
        size: PAGE_SIZE
      };

      let response: UserListResponse;
      
      if (searchId && searchId.trim()) {
        response = await userService.searchUsersFromId(searchId.trim(), params);
      } else {
        response = await userService.getUsers(params);
      }
      
      updateState({
        users: response.users || [],
        pagination: response,
        isLoading: false,
        error: null,
        currentPage: page
      });

      if (response.users.length === 0) {
        if (page > 0) {
          setMessage('You are at the bottom of the page...', 'info');
        } else if (searchId && searchId.trim()) {
          setMessage('You are at the top of the page...', 'info');
        } else {
          setMessage('No users found', 'info');
        }
      } else {
        setMessage('', '');
      }
    } catch (error) {
      const errorMessage = error instanceof Error ? error.message : 'Unable to lookup User...';
      updateState({
        users: [],
        pagination: null,
        isLoading: false,
        error: errorMessage
      });
      setMessage(errorMessage, 'error');
    }
  }, [updateState, setMessage]);

  const handleEnter = useCallback(() => {
    // Check for user selections first
    const selectedEntries = Object.entries(state.selectedActions).filter(([_, action]) => action && action.trim() !== '');
    
    if (selectedEntries.length > 0) {
      const [userId, action] = selectedEntries[0]; // Take first selection
      const normalizedAction = action.toUpperCase();
      
      if (normalizedAction === 'U') {
        // Navigate to update user page
        router.push(`/users/${userId}/edit`);
        return;
      } else if (normalizedAction === 'D') {
        // Navigate to delete user page
        router.push(`/users/${userId}/delete`);
        return;
      } else {
        setMessage('Invalid selection. Valid values are U and D', 'error');
        return;
      }
    }
    
    // If no selection, perform search
    if (state.searchUserId.trim()) {
      fetchUsers(0, state.searchUserId);
    } else {
      fetchUsers(0);
    }
  }, [state.selectedActions, state.searchUserId, fetchUsers, setMessage, router]);

  const handlePF3Exit = useCallback(() => {
    router.push('/admin');
  }, [router]);

  const handlePF4Clear = useCallback(() => {
    updateState({
      searchUserId: '',
      selectedActions: {},
      message: '',
      messageType: ''
    });
  }, [updateState]);

  const handlePF7Previous = useCallback(() => {
    if (state.currentPage > 0) {
      const prevPage = state.currentPage - 1;
      fetchUsers(prevPage, state.searchUserId || undefined);
    } else {
      setMessage('You are already at the top of the page...', 'info');
    }
  }, [state.currentPage, state.searchUserId, fetchUsers, setMessage]);

  const handlePF8Next = useCallback(() => {
    if (state.pagination && !state.pagination.last) {
      const nextPage = state.currentPage + 1;
      fetchUsers(nextPage, state.searchUserId || undefined);
    } else {
      setMessage('You are already at the bottom of the page...', 'info');
    }
  }, [state.pagination, state.currentPage, state.searchUserId, fetchUsers, setMessage]);

  const handleActionChange = useCallback((userId: string, action: string) => {
    updateState({
      selectedActions: {
        ...state.selectedActions,
        [userId]: action
      }
    });
  }, [state.selectedActions, updateState]);

  const handleKeyPress = useCallback((event: KeyboardEvent) => {
    switch (event.key) {
      case 'Enter':
        event.preventDefault();
        handleEnter();
        break;
      case 'F3':
        event.preventDefault();
        handlePF3Exit();
        break;
      case 'F4':
        event.preventDefault();
        handlePF4Clear();
        break;
      case 'F7':
        event.preventDefault();
        handlePF7Previous();
        break;
      case 'F8':
        event.preventDefault();
        handlePF8Next();
        break;
      default:
        // Check for other keys and show invalid key message
        if (event.key.length === 1 && !event.ctrlKey && !event.altKey && !event.metaKey) {
          // Only show invalid key for function keys, not regular typing
          return;
        }
        if (event.key.startsWith('F') && event.key !== 'F3' && event.key !== 'F4' && event.key !== 'F7' && event.key !== 'F8') {
          setMessage('INVALID KEY PRESSED', 'error');
        }
        break;
    }
  }, [handleEnter, handlePF3Exit, handlePF4Clear, handlePF7Previous, handlePF8Next, setMessage]);

  // Add keyboard event listener
  useEffect(() => {
    document.addEventListener('keydown', handleKeyPress);
    return () => {
      document.removeEventListener('keydown', handleKeyPress);
    };
  }, [handleKeyPress]);

  // Load initial data
  useEffect(() => {
    fetchUsers(0);
  }, [fetchUsers]);

  const tableColumns = [
    { key: 'sel' as const, label: 'Sel', width: '60px' },
    { key: 'userId' as const, label: 'User ID', width: '120px' },
    { key: 'firstName' as const, label: 'First Name', width: '200px' },
    { key: 'lastName' as const, label: 'Last Name', width: '200px' },
    { key: 'userType' as const, label: 'User Type', width: '100px' }
  ];

  const tableData = state.users.map(user => ({
    sel: (
      <Input
        type="text"
        value={state.selectedActions[user.userId] || ''}
        onChange={(e) => handleActionChange(user.userId, e.target.value)}
        className="w-12 h-8 text-center text-sm"
        maxLength={1}
        placeholder=""
      />
    ),
    userId: user.userId,
    firstName: user.firstName,
    lastName: user.lastName,
    userType: user.userType === 'A' ? 'Admin' : 'Regular'
  }));

  const currentPage = state.pagination?.page ?? 0;
  const totalPages = state.pagination?.totalPages ?? 0;
  const totalElements = state.pagination?.totalElements ?? 0;

  return (
    <div className="min-h-screen bg-gray-50 p-6">
      <div className="max-w-7xl mx-auto">
        {/* Header */}
        <div className="bg-white rounded-lg shadow-sm border border-gray-200 p-6 mb-6">
          <div className="flex justify-between items-center">
            <div>
              <h1 className="text-2xl font-bold text-gray-900 mb-2">CARDDEMO</h1>
              <h2 className="text-lg text-gray-700">User List</h2>
            </div>
            <div className="text-right text-sm text-gray-600">
              <div>Program: COUSR00C</div>
              <div>Transaction: CU00</div>
              <div>{new Date().toLocaleDateString()}</div>
              <div>{new Date().toLocaleTimeString()}</div>
            </div>
          </div>
        </div>

        {/* Search Section */}
        <div className="bg-white rounded-lg shadow-sm border border-gray-200 p-6 mb-6">
          <div className="flex items-center gap-4">
            <label htmlFor="searchUserId" className="text-sm font-medium text-gray-700 whitespace-nowrap">
              Search from User ID:
            </label>
            <Input
              id="searchUserId"
              type="text"
              value={state.searchUserId}
              onChange={(e) => updateState({ searchUserId: e.target.value.toUpperCase() })}
              className="w-32"
              maxLength={8}
              disabled={state.isLoading}
            />
          </div>
        </div>

        {/* Message Display */}
        {state.message && (
          <div className={`mb-6 p-4 rounded-lg border ${
            state.messageType === 'success' ? 'bg-green-50 border-green-200 text-green-800' :
            state.messageType === 'error' ? 'bg-red-50 border-red-200 text-red-800' :
            'bg-blue-50 border-blue-200 text-blue-800'
          }`}>
            {state.message}
          </div>
        )}

        {/* User List Table */}
        <div className="bg-white rounded-lg shadow-sm border border-gray-200 mb-6">
          <div className="p-6 border-b border-gray-200">
            <div className="flex justify-between items-center">
              <h3 className="text-lg font-semibold text-gray-900">Users</h3>
              <div className="text-sm text-gray-600">
                Page {currentPage + 1} of {totalPages}
              </div>
            </div>
          </div>
          
          <div className="overflow-x-auto">
            {state.isLoading ? (
              <div className="flex justify-center items-center py-12">
                <div className="animate-spin rounded-full h-8 w-8 border-b-2 border-blue-600"></div>
                <span className="ml-3 text-gray-600">Loading users...</span>
              </div>
            ) : state.error ? (
              <div className="text-center py-12">
                <p className="text-red-600 mb-4">{state.error}</p>
                <Button onClick={() => fetchUsers(0)} variant="outline">
                  Retry
                </Button>
              </div>
            ) : state.users.length === 0 ? (
              <div className="text-center py-12">
                <p className="text-gray-500">No users found</p>
              </div>
            ) : (
              <Table
                columns={tableColumns}
                data={tableData}
                className="w-full"
              />
            )}
          </div>
        </div>

        {/* Instructions */}
        <div className="bg-blue-50 border border-blue-200 rounded-lg p-4 mb-6">
          <h4 className="font-semibold text-blue-900 mb-2">Instructions:</h4>
          <ul className="text-sm text-blue-800 space-y-1">
            <li>• Enter a User ID in the search field and press Enter to search</li>
            <li>• Type 'U' in the Sel column to update a user, 'D' to delete a user, then press Enter</li>
            <li>• Use F7 for Previous Page, F8 for Next Page</li>
            <li>• Press F3 to exit to Admin Menu, F4 to clear the screen</li>
          </ul>
        </div>

        {/* Function Keys */}
        <div className="bg-white rounded-lg shadow-sm border border-gray-200 p-6">
          <div className="flex flex-wrap justify-between items-center gap-4">
            <div className="flex gap-2">
              <Button
                onClick={handleEnter}
                disabled={state.isLoading}
                className="bg-blue-600 hover:bg-blue-700 text-white"
              >
                Enter
              </Button>
              <Button
                onClick={handlePF7Previous}
                disabled={state.isLoading || state.currentPage === 0}
                variant="outline"
              >
                F7 - Previous
              </Button>
              <Button
                onClick={handlePF8Next}
                disabled={state.isLoading || (state.pagination?.last ?? true)}
                variant="outline"
              >
                F8 - Next
              </Button>
            </div>

            <div className="flex gap-2">
              <Button
                onClick={handlePF4Clear}
                disabled={state.isLoading}
                variant="outline"
              >
                F4 - Clear
              </Button>
              <Button
                onClick={handlePF3Exit}
                disabled={state.isLoading}
                variant="outline"
                className="text-gray-600 hover:text-gray-800"
              >
                F3 - Exit
              </Button>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}