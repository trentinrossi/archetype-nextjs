'use client';

import React, { useState, useEffect, useCallback } from 'react';
import { useRouter, useParams } from 'next/navigation';
import { Button, Input } from '@/components/ui';
import { userService } from '@/services/userService';
import { User } from '@/types/user';

interface DeleteUserPageState {
  lookupUserId: string;
  user: User | null;
  isLoading: boolean;
  isLookingUp: boolean;
  isDeleting: boolean;
  message: string;
  messageType: 'success' | 'error' | 'info' | '';
  mode: 'lookup' | 'confirm';
}

const INITIAL_STATE: DeleteUserPageState = {
  lookupUserId: '',
  user: null,
  isLoading: false,
  isLookingUp: false,
  isDeleting: false,
  message: '',
  messageType: '',
  mode: 'lookup'
};

const MAX_LENGTHS = {
  userId: 8
};

const getUserTypeLabel = (userType: string): string => {
  switch (userType) {
    case 'A':
      return 'Admin';
    case 'R':
      return 'Regular';
    default:
      return userType;
  }
};

export default function DeleteUserPage() {
  const router = useRouter();
  const params = useParams();
  const [state, setState] = useState<DeleteUserPageState>(INITIAL_STATE);

  const updateState = useCallback((updates: Partial<DeleteUserPageState>) => {
    setState(prev => ({ ...prev, ...updates }));
  }, []);

  const setMessage = useCallback((message: string, type: 'success' | 'error' | 'info' = 'info') => {
    updateState({ message, messageType: type });
  }, [updateState]);

  const handleLookupInputChange = useCallback((value: string) => {
    const processedValue = value.slice(0, MAX_LENGTHS.userId).toUpperCase();
    updateState({ lookupUserId: processedValue });
  }, [updateState]);

  const handleEnter = useCallback(async () => {
    if (state.mode === 'lookup') {
      // Lookup user
      if (!state.lookupUserId.trim()) {
        setMessage('User ID can NOT be empty...', 'error');
        return;
      }

      updateState({ isLookingUp: true });

      try {
        const user = await userService.getUserById(state.lookupUserId.trim());
        
        updateState({
          user,
          mode: 'confirm',
          isLookingUp: false
        });

        setMessage('Press PF5 key to delete this user ...', 'info');

      } catch (error) {
        const errorMessage = error instanceof Error ? error.message : 'Unable to lookup User...';
        
        if (errorMessage.toLowerCase().includes('not found') || errorMessage.toLowerCase().includes('404')) {
          setMessage('User ID NOT found...', 'error');
        } else {
          setMessage('Unable to lookup User...', 'error');
        }
        
        updateState({ isLookingUp: false });
      }
    }
  }, [state.mode, state.lookupUserId, updateState, setMessage]);

  const handlePF5Delete = useCallback(async () => {
    if (!state.user) {
      setMessage('User ID can NOT be empty...', 'error');
      return;
    }

    updateState({ isDeleting: true });

    try {
      // First, read the user again to ensure it still exists and lock it
      await userService.getUserById(state.user.userId);
      
      // Then delete the user
      await userService.deleteUser(state.user.userId);
      
      // Clear all fields and show success message
      updateState({
        lookupUserId: '',
        user: null,
        mode: 'lookup',
        isDeleting: false
      });
      
      setMessage(`User ${state.user.userId} has been deleted ...`, 'success');
      
      // Focus on User ID field for next operation
      setTimeout(() => {
        const element = document.getElementById('lookupUserId');
        if (element) {
          element.focus();
        }
      }, 100);

    } catch (error) {
      const errorMessage = error instanceof Error ? error.message : 'Unable to Update User...';
      
      if (errorMessage.toLowerCase().includes('not found') || errorMessage.toLowerCase().includes('404')) {
        setMessage('User ID NOT found...', 'error');
        // Focus on User ID field
        const element = document.getElementById('lookupUserId');
        if (element) {
          element.focus();
        }
      } else {
        setMessage('Unable to Update User...', 'error');
        // Focus on First Name field for general errors (following the pattern)
        const element = document.getElementById('firstName');
        if (element) {
          element.focus();
        }
      }
      
      updateState({ isDeleting: false });
    }
  }, [state.user, updateState, setMessage]);

  const handlePF3Exit = useCallback(() => {
    router.push('/admin');
  }, [router]);

  const handlePF4Clear = useCallback(() => {
    if (state.mode === 'lookup') {
      updateState({
        lookupUserId: '',
        message: '',
        messageType: ''
      });
    } else {
      updateState({
        lookupUserId: '',
        user: null,
        mode: 'lookup',
        message: '',
        messageType: ''
      });
    }
  }, [state.mode, updateState]);

  const handlePF12Cancel = useCallback(() => {
    router.push('/admin');
  }, [router]);

  const handleKeyDown = useCallback((event: KeyboardEvent) => {
    switch (event.key) {
      case 'Enter':
        event.preventDefault();
        if (!state.isLoading && !state.isLookingUp && !state.isDeleting) {
          handleEnter();
        }
        break;
      
      case 'F3':
        event.preventDefault();
        handlePF3Exit();
        break;
      
      case 'F4':
        event.preventDefault();
        handlePF4Clear();
        break;
      
      case 'F5':
        event.preventDefault();
        if (state.mode === 'confirm' && !state.isDeleting) {
          handlePF5Delete();
        }
        break;
      
      case 'F12':
        event.preventDefault();
        handlePF12Cancel();
        break;
      
      default:
        if (event.key.startsWith('F') && !['F3', 'F4', 'F5', 'F12'].includes(event.key)) {
          event.preventDefault();
          setMessage('INVALID KEY PRESSED', 'error');
        }
        break;
    }
  }, [state.isLoading, state.isLookingUp, state.isDeleting, state.mode, handleEnter, handlePF3Exit, handlePF4Clear, handlePF5Delete, handlePF12Cancel, setMessage]);

  // Set up keyboard event listeners
  useEffect(() => {
    document.addEventListener('keydown', handleKeyDown);
    return () => {
      document.removeEventListener('keydown', handleKeyDown);
    };
  }, [handleKeyDown]);

  // Check if userId is provided in URL params
  useEffect(() => {
    if (params.userId && typeof params.userId === 'string') {
      updateState({ lookupUserId: params.userId });
      // Auto-lookup if userId is provided
      setTimeout(() => {
        handleEnter();
      }, 100);
    }
  }, [params.userId, updateState]);

  // Focus on lookup field on mount
  useEffect(() => {
    if (state.mode === 'lookup') {
      const element = document.getElementById('lookupUserId');
      if (element) {
        element.focus();
      }
    }
  }, [state.mode]);

  return (
    <div className="min-h-screen bg-gray-50 p-6">
      <div className="max-w-2xl mx-auto">
        {/* Header */}
        <div className="bg-white rounded-lg shadow-sm border border-gray-200 p-6 mb-6">
          <div className="flex justify-between items-center">
            <div>
              <h1 className="text-2xl font-bold text-gray-900 mb-2">CARDDEMO</h1>
              <h2 className="text-lg text-gray-700">Delete a User</h2>
            </div>
            <div className="text-right text-sm text-gray-600">
              <div>Program: COUSR03C</div>
              <div>Transaction: CU03</div>
              <div>{new Date().toLocaleDateString()}</div>
              <div>{new Date().toLocaleTimeString()}</div>
            </div>
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

        {/* Lookup Section */}
        {state.mode === 'lookup' && (
          <div className="bg-white rounded-lg shadow-sm border border-gray-200 p-6 mb-6">
            <div className="space-y-4">
              <div>
                <label htmlFor="lookupUserId" className="block text-sm font-medium text-gray-700 mb-2">
                  User ID <span className="text-red-500">*</span>
                </label>
                <Input
                  id="lookupUserId"
                  type="text"
                  value={state.lookupUserId}
                  onChange={(e) => handleLookupInputChange(e.target.value)}
                  placeholder="Enter User ID to delete"
                  className="w-full"
                  disabled={state.isLookingUp}
                  maxLength={MAX_LENGTHS.userId}
                />
              </div>
            </div>
          </div>
        )}

        {/* Confirmation Section */}
        {state.mode === 'confirm' && state.user && (
          <div className="bg-white rounded-lg shadow-sm border border-red-200 p-6 mb-6">
            <div className="flex items-center gap-2 mb-4">
              <span className="text-red-600 text-xl">⚠️</span>
              <h3 className="text-lg font-semibold text-red-900">Confirm User Deletion</h3>
            </div>
            
            <div className="bg-red-50 border border-red-200 rounded-lg p-4 mb-6">
              <p className="text-red-800 font-medium mb-2">
                You are about to permanently delete the following user:
              </p>
              <p className="text-red-700 text-sm">
                This action cannot be undone.
              </p>
            </div>

            <div className="space-y-4">
              <div className="grid grid-cols-2 gap-4">
                <div>
                  <label className="block text-sm font-medium text-gray-700 mb-1">
                    User ID
                  </label>
                  <div className="p-3 bg-gray-50 border border-gray-200 rounded-md">
                    <span className="text-gray-900 font-mono">{state.user.userId}</span>
                  </div>
                </div>

                <div>
                  <label className="block text-sm font-medium text-gray-700 mb-1">
                    User Type
                  </label>
                  <div className="p-3 bg-gray-50 border border-gray-200 rounded-md">
                    <span className="text-gray-900">{getUserTypeLabel(state.user.userType)}</span>
                  </div>
                </div>
              </div>

              <div className="grid grid-cols-2 gap-4">
                <div>
                  <label className="block text-sm font-medium text-gray-700 mb-1">
                    First Name
                  </label>
                  <div className="p-3 bg-gray-50 border border-gray-200 rounded-md">
                    <span className="text-gray-900">{state.user.firstName}</span>
                  </div>
                </div>

                <div>
                  <label className="block text-sm font-medium text-gray-700 mb-1">
                    Last Name
                  </label>
                  <div className="p-3 bg-gray-50 border border-gray-200 rounded-md">
                    <span className="text-gray-900">{state.user.lastName}</span>
                  </div>
                </div>
              </div>
            </div>
          </div>
        )}

        {/* Action Buttons */}
        <div className="bg-white rounded-lg shadow-sm border border-gray-200 p-6 mb-6">
          <div className="flex flex-wrap gap-4 justify-between">
            <div className="flex gap-3">
              {state.mode === 'lookup' ? (
                <Button
                  onClick={handleEnter}
                  disabled={state.isLookingUp || !state.lookupUserId.trim()}
                  className="bg-blue-600 hover:bg-blue-700 text-white"
                >
                  {state.isLookingUp ? 'Looking up...' : 'Enter - Lookup'}
                </Button>
              ) : (
                <Button
                  onClick={handlePF5Delete}
                  disabled={state.isDeleting}
                  className="bg-red-600 hover:bg-red-700 text-white"
                >
                  {state.isDeleting ? 'Deleting...' : 'F5 - Delete User'}
                </Button>
              )}
              
              <Button
                onClick={handlePF4Clear}
                disabled={state.isLookingUp || state.isDeleting}
                variant="outline"
              >
                F4 - Clear
              </Button>
            </div>

            <div className="flex gap-2">
              <Button
                onClick={handlePF3Exit}
                disabled={state.isLookingUp || state.isDeleting}
                variant="outline"
              >
                F3 - Exit
              </Button>
              <Button
                onClick={handlePF12Cancel}
                disabled={state.isLookingUp || state.isDeleting}
                variant="outline"
                className="text-gray-600 hover:text-gray-800"
              >
                F12 - Cancel
              </Button>
            </div>
          </div>
        </div>

        {/* Instructions */}
        <div className="bg-blue-50 border border-blue-200 rounded-lg p-4">
          <h4 className="font-semibold text-blue-900 mb-2">Instructions:</h4>
          <ul className="text-sm text-blue-800 space-y-1">
            {state.mode === 'lookup' ? (
              <>
                <li>• Enter a User ID and press Enter to lookup</li>
                <li>• Press F3 to exit to Admin Menu</li>
                <li>• Press F4 to clear the lookup field</li>
                <li>• Press F12 to cancel and return to Admin Menu</li>
              </>
            ) : (
              <>
                <li>• Review the user information carefully</li>
                <li>• Press F5 to confirm and delete the user (PERMANENT)</li>
                <li>• Press F3 to exit to Admin Menu</li>
                <li>• Press F4 to clear and start over</li>
                <li>• Press F12 to cancel without deleting</li>
              </>
            )}
          </ul>
        </div>
      </div>
    </div>
  );
}