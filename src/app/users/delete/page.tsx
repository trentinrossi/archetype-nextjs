'use client';

import { useState, useEffect, useCallback } from 'react';
import { useRouter, useSearchParams } from 'next/navigation';
import { Button, Input } from '@/components/ui';
import userSecurityService from '@/services/userSecurityService';
import {
  UserSecurity,
  InvalidKeyRequest
} from '@/types/userSecurity';

interface UserDeleteState {
  isLoading: boolean;
  isLookingUp: boolean;
  isDeleting: boolean;
  error: string;
  message: string;
  currentDateTime: string;
  userFound: boolean;
  confirmationMode: boolean;
  lookupUserId: string;
  foundUser: UserSecurity | null;
}

export default function UserDeletePage() {
  const router = useRouter();
  const searchParams = useSearchParams();
  const prefilledUserId = searchParams.get('userId') || '';

  const [state, setState] = useState<UserDeleteState>({
    isLoading: false,
    isLookingUp: false,
    isDeleting: false,
    error: '',
    message: '',
    currentDateTime: '',
    userFound: false,
    confirmationMode: false,
    lookupUserId: prefilledUserId,
    foundUser: null
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

  // Initialize page
  useEffect(() => {
    updateDateTime();
    
    // Update time every second
    const interval = setInterval(updateDateTime, 1000);
    
    setState(prev => ({ 
      ...prev, 
      message: 'Enter User ID and press Enter to lookup user for deletion.' 
    }));

    // If userId is prefilled, automatically lookup the user
    if (prefilledUserId) {
      handleUserLookup(prefilledUserId);
    }

    return () => clearInterval(interval);
  }, [updateDateTime, prefilledUserId]);

  // Handle user lookup
  const handleUserLookup = useCallback(async (userId?: string): Promise<void> => {
    const userIdToLookup = userId || state.lookupUserId.trim();
    
    if (!userIdToLookup) {
      setState(prev => ({ 
        ...prev, 
        error: 'Please enter a User ID to lookup.' 
      }));
      return;
    }

    setState(prev => ({ 
      ...prev, 
      isLookingUp: true, 
      error: '', 
      message: `Looking up user: ${userIdToLookup}...`,
      userFound: false,
      confirmationMode: false
    }));

    try {
      const response = await userSecurityService.getUserById(userIdToLookup);

      if (response.success && response.data?.user) {
        const user = response.data.user;
        
        setState(prev => ({
          ...prev,
          isLookingUp: false,
          userFound: true,
          confirmationMode: true,
          foundUser: user,
          message: `User found: ${user.firstName} ${user.lastName}. Press F5 to confirm deletion.`
        }));
      } else {
        setState(prev => ({
          ...prev,
          isLookingUp: false,
          userFound: false,
          confirmationMode: false,
          foundUser: null,
          error: response.error || `User not found: ${userIdToLookup}`
        }));
      }
    } catch (error) {
      console.error('User lookup error:', error);
      setState(prev => ({
        ...prev,
        isLookingUp: false,
        userFound: false,
        confirmationMode: false,
        foundUser: null,
        error: error instanceof Error ? error.message : 'Failed to lookup user'
      }));
    }
  }, [state.lookupUserId]);

  // Handle delete confirmation (PF5)
  const handleDeleteConfirmation = useCallback(async (): Promise<void> => {
    if (!state.foundUser) {
      setState(prev => ({ ...prev, error: 'No user selected for deletion.' }));
      return;
    }

    const confirmDelete = window.confirm(
      `Are you sure you want to delete user "${state.foundUser.firstName} ${state.foundUser.lastName}" (${state.foundUser.username})?\n\nThis action cannot be undone.`
    );

    if (!confirmDelete) {
      setState(prev => ({ ...prev, message: 'Deletion cancelled by user.' }));
      return;
    }

    setState(prev => ({ 
      ...prev, 
      isDeleting: true, 
      error: '', 
      message: `Deleting user: ${prev.foundUser?.firstName} ${prev.foundUser?.lastName}...` 
    }));

    try {
      const response = await userSecurityService.deleteUser(state.foundUser.userId);

      if (response.success) {
        setState(prev => ({
          ...prev,
          isDeleting: false,
          userFound: false,
          confirmationMode: false,
          foundUser: null,
          lookupUserId: '',
          message: `User "${prev.foundUser?.firstName} ${prev.foundUser?.lastName}" has been successfully deleted.`,
          error: ''
        }));
      } else {
        setState(prev => ({
          ...prev,
          isDeleting: false,
          error: response.error || 'Failed to delete user'
        }));
      }
    } catch (error) {
      console.error('User deletion error:', error);
      setState(prev => ({
        ...prev,
        isDeleting: false,
        error: error instanceof Error ? error.message : 'Failed to delete user'
      }));
    }
  }, [state.foundUser]);

  // Handle clear screen (PF4)
  const handleClear = useCallback((): void => {
    setState(prev => ({
      ...prev,
      lookupUserId: '',
      userFound: false,
      confirmationMode: false,
      foundUser: null,
      error: '',
      message: 'Screen cleared. Enter User ID to lookup another user.'
    }));
  }, []);

  // Handle exit (PF3 or PF12)
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
          error: `Invalid key pressed: ${key}. Use Enter to lookup, PF3 (Exit), PF4 (Clear), PF5 (Delete), or PF12 (Exit).` 
        }));
      }
    } catch (error) {
      console.error('Invalid key handling error:', error);
      setState(prev => ({ 
        ...prev, 
        error: `Invalid key pressed: ${key}. Use Enter to lookup, PF3 (Exit), PF4 (Clear), PF5 (Delete), or PF12 (Exit).` 
      }));
    }
  }, []);

  // Handle keyboard events
  const handleKeyDown = useCallback(async (event: React.KeyboardEvent): Promise<void> => {
    if (state.isLoading || state.isLookingUp || state.isDeleting) {
      return;
    }

    // Clear previous messages for function keys
    if (event.key.startsWith('F') || event.key === 'Enter') {
      event.preventDefault();
      setState(prev => ({ ...prev, error: '', message: '' }));
    }

    switch (event.key) {
      case 'Enter':
        if (!state.userFound) {
          await handleUserLookup();
        }
        break;
      
      case 'F3':
        handleExit();
        break;
      
      case 'F4':
        handleClear();
        break;
      
      case 'F5':
        if (state.confirmationMode) {
          await handleDeleteConfirmation();
        } else {
          await handleInvalidKey('F5 (Delete not available - no user selected)');
        }
        break;
      
      case 'F12':
        handleExit();
        break;
      
      case 'F1':
      case 'F2':
      case 'F6':
      case 'F7':
      case 'F8':
      case 'F9':
      case 'F10':
      case 'F11':
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
  }, [state.isLoading, state.isLookingUp, state.isDeleting, state.userFound, state.confirmationMode, handleUserLookup, handleExit, handleClear, handleDeleteConfirmation, handleInvalidKey]);

  return (
    <div 
      className="min-h-screen bg-gray-50 py-8 px-4 sm:px-6 lg:px-8"
      onKeyDown={handleKeyDown}
      tabIndex={-1}
    >
      <div className="max-w-4xl mx-auto">
        {/* Header */}
        <div className="bg-white shadow rounded-lg p-6 mb-8">
          <div className="flex justify-between items-center">
            <div>
              <h1 className="text-3xl font-bold text-gray-900">User Deletion Program</h1>
              <p className="mt-1 text-sm text-gray-600">Transaction ID: COUSR03C</p>
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

        {/* User Lookup Section */}
        <div className="bg-white shadow rounded-lg p-6 mb-8">
          <h2 className="text-xl font-semibold text-gray-900 mb-4">User Lookup</h2>
          
          <div className="flex items-end space-x-4">
            <div className="flex-1">
              <label htmlFor="lookupUserId" className="block text-sm font-medium text-gray-700 mb-2">
                User ID
              </label>
              <Input
                id="lookupUserId"
                type="text"
                value={state.lookupUserId}
                onChange={(e) => setState(prev => ({ ...prev, lookupUserId: e.target.value }))}
                placeholder="Enter User ID to lookup"
                disabled={state.isLookingUp || state.userFound}
                className="w-full"
              />
            </div>
            <Button
              onClick={() => handleUserLookup()}
              disabled={state.isLookingUp || !state.lookupUserId.trim()}
              className="px-6 py-2"
            >
              {state.isLookingUp ? 'Looking up...' : 'Lookup (Enter)'}
            </Button>
          </div>
        </div>

        {/* User Details Display */}
        {state.userFound && state.foundUser && (
          <div className="bg-white shadow rounded-lg p-6 mb-8">
            <div className="flex justify-between items-center mb-6">
              <h2 className="text-xl font-semibold text-gray-900">User Details</h2>
              {state.confirmationMode && (
                <span className="inline-flex items-center px-3 py-1 rounded-full text-sm font-medium bg-red-100 text-red-800">
                  Ready for Deletion
                </span>
              )}
            </div>

            <div className="grid gap-6 md:grid-cols-2">
              {/* User ID */}
              <div>
                <label className="block text-sm font-medium text-gray-700 mb-2">
                  User ID
                </label>
                <div className="px-3 py-2 bg-gray-50 border border-gray-300 rounded-md text-sm text-gray-900">
                  {state.foundUser.userId}
                </div>
              </div>

              {/* Username */}
              <div>
                <label className="block text-sm font-medium text-gray-700 mb-2">
                  Username
                </label>
                <div className="px-3 py-2 bg-gray-50 border border-gray-300 rounded-md text-sm text-gray-900">
                  {state.foundUser.username}
                </div>
              </div>

              {/* First Name */}
              <div>
                <label className="block text-sm font-medium text-gray-700 mb-2">
                  First Name
                </label>
                <div className="px-3 py-2 bg-gray-50 border border-gray-300 rounded-md text-sm text-gray-900">
                  {state.foundUser.firstName}
                </div>
              </div>

              {/* Last Name */}
              <div>
                <label className="block text-sm font-medium text-gray-700 mb-2">
                  Last Name
                </label>
                <div className="px-3 py-2 bg-gray-50 border border-gray-300 rounded-md text-sm text-gray-900">
                  {state.foundUser.lastName}
                </div>
              </div>

              {/* Email */}
              <div>
                <label className="block text-sm font-medium text-gray-700 mb-2">
                  Email Address
                </label>
                <div className="px-3 py-2 bg-gray-50 border border-gray-300 rounded-md text-sm text-gray-900">
                  {state.foundUser.email}
                </div>
              </div>

              {/* User Type/Roles */}
              <div>
                <label className="block text-sm font-medium text-gray-700 mb-2">
                  User Type
                </label>
                <div className="px-3 py-2 bg-gray-50 border border-gray-300 rounded-md text-sm text-gray-900">
                  {state.foundUser.roles.join(', ')}
                </div>
              </div>

              {/* Status */}
              <div>
                <label className="block text-sm font-medium text-gray-700 mb-2">
                  Status
                </label>
                <div className="px-3 py-2 bg-gray-50 border border-gray-300 rounded-md text-sm">
                  <span className={`inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium ${
                    state.foundUser.isActive 
                      ? 'bg-green-100 text-green-800' 
                      : 'bg-red-100 text-red-800'
                  }`}>
                    {state.foundUser.isActive ? 'Active' : 'Inactive'}
                  </span>
                </div>
              </div>

              {/* Created Date */}
              <div>
                <label className="block text-sm font-medium text-gray-700 mb-2">
                  Created Date
                </label>
                <div className="px-3 py-2 bg-gray-50 border border-gray-300 rounded-md text-sm text-gray-900">
                  {new Date(state.foundUser.createdAt).toLocaleDateString()}
                </div>
              </div>
            </div>

            {/* Deletion Warning */}
            {state.confirmationMode && (
              <div className="mt-6 p-4 bg-red-50 border border-red-200 rounded-md">
                <div className="flex">
                  <div className="flex-shrink-0">
                    <svg className="h-5 w-5 text-red-400" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 20 20" fill="currentColor">
                      <path fillRule="evenodd" d="M8.257 3.099c.765-1.36 2.722-1.36 3.486 0l5.58 9.92c.75 1.334-.213 2.98-1.742 2.98H4.42c-1.53 0-2.493-1.646-1.743-2.98l5.58-9.92zM11 13a1 1 0 11-2 0 1 1 0 012 0zm-1-8a1 1 0 00-1 1v3a1 1 0 002 0V6a1 1 0 00-1-1z" clipRule="evenodd" />
                    </svg>
                  </div>
                  <div className="ml-3">
                    <h3 className="text-sm font-medium text-red-800">
                      Warning: User Deletion
                    </h3>
                    <div className="mt-2 text-sm text-red-700">
                      <p>
                        You are about to permanently delete this user account. This action cannot be undone.
                        Press <kbd className="px-1 py-0.5 text-xs font-mono bg-red-200 rounded">F5</kbd> to confirm deletion.
                      </p>
                    </div>
                  </div>
                </div>
              </div>
            )}
          </div>
        )}

        {/* Action Buttons */}
        <div className="flex justify-between">
          <div className="flex space-x-4">
            <Button
              onClick={handleExit}
              disabled={state.isLoading || state.isLookingUp || state.isDeleting}
              variant="secondary"
              className="px-6 py-2"
            >
              Exit (F3)
            </Button>

            <Button
              onClick={handleClear}
              disabled={state.isLoading || state.isLookingUp || state.isDeleting}
              variant="secondary"
              className="px-6 py-2"
            >
              Clear (F4)
            </Button>

            <Button
              onClick={handleDeleteConfirmation}
              disabled={state.isLoading || state.isLookingUp || state.isDeleting || !state.confirmationMode}
              variant="primary"
              className="px-6 py-2 bg-red-600 hover:bg-red-700 focus:ring-red-500"
            >
              {state.isDeleting ? 'Deleting...' : 'Delete User (F5)'}
            </Button>
          </div>

          <Button
            onClick={handleExit}
            disabled={state.isLoading || state.isLookingUp || state.isDeleting}
            variant="secondary"
            className="px-6 py-2"
          >
            Exit (F12)
          </Button>
        </div>

        {/* Instructions */}
        <div className="mt-8 bg-gray-50 rounded-lg p-6">
          <h3 className="text-lg font-medium text-gray-900 mb-4">Program Instructions</h3>
          
          <div className="grid gap-6 md:grid-cols-2">
            <div>
              <h4 className="text-sm font-medium text-gray-900 mb-2">User Lookup:</h4>
              <ul className="text-sm text-gray-600 space-y-1">
                <li>• Enter User ID in the lookup field</li>
                <li>• Press <kbd className="px-1 py-0.5 text-xs font-mono bg-gray-200 rounded">Enter</kbd> to lookup user</li>
                <li>• User details will be displayed if found</li>
                <li>• Error message shown if user not found</li>
              </ul>
            </div>
            
            <div>
              <h4 className="text-sm font-medium text-gray-900 mb-2">Deletion Process:</h4>
              <ul className="text-sm text-gray-600 space-y-1">
                <li>• Review user details carefully</li>
                <li>• Press <kbd className="px-1 py-0.5 text-xs font-mono bg-gray-200 rounded">F5</kbd> to confirm deletion</li>
                <li>• Additional confirmation dialog will appear</li>
                <li>• Deletion is permanent and cannot be undone</li>
              </ul>
            </div>
          </div>

          <div className="mt-4">
            <h4 className="text-sm font-medium text-gray-900 mb-2">Function Keys:</h4>
            <div className="grid grid-cols-2 gap-4 text-sm text-gray-600">
              <div>
                <li>• <kbd className="px-1 py-0.5 text-xs font-mono bg-gray-200 rounded">F3</kbd> - Exit program</li>
                <li>• <kbd className="px-1 py-0.5 text-xs font-mono bg-gray-200 rounded">F4</kbd> - Clear screen and start over</li>
              </div>
              <div>
                <li>• <kbd className="px-1 py-0.5 text-xs font-mono bg-gray-200 rounded">F5</kbd> - Confirm user deletion</li>
                <li>• <kbd className="px-1 py-0.5 text-xs font-mono bg-gray-200 rounded">F12</kbd> - Exit program</li>
              </div>
            </div>
          </div>

          <div className="mt-4 p-3 bg-red-50 rounded-md border border-red-200">
            <p className="text-sm text-red-800">
              <strong>Warning:</strong> This program permanently deletes user accounts from the system. 
              Ensure you have selected the correct user before confirming deletion. This action cannot be reversed.
            </p>
          </div>
        </div>
      </div>
    </div>
  );
}