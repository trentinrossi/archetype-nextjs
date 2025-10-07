'use client';

import React, { useState, useRef, useEffect } from 'react';
import { useRouter } from 'next/navigation';
import { userSecurityService } from '@/services/userSecurityService';
import { UserSecurityDTO, UserSecurityActionResult } from '@/types/userSecurity';

interface UserDeleteComponentProps {
  userId?: string;
  onUserDeleted?: (userId: string) => void;
  onExit?: () => void;
  className?: string;
}

const UserDeleteComponent: React.FC<UserDeleteComponentProps> = ({
  userId: preselectedUserId,
  onUserDeleted,
  onExit,
  className = ''
}) => {
  const router = useRouter();
  const [lookupUserId, setLookupUserId] = useState<string>(preselectedUserId || '');
  const [currentUser, setCurrentUser] = useState<UserSecurityDTO | null>(null);
  const [isLoading, setIsLoading] = useState<boolean>(false);
  const [isLookingUp, setIsLookingUp] = useState<boolean>(false);
  const [successMessage, setSuccessMessage] = useState<string>('');
  const [generalError, setGeneralError] = useState<string>('');
  const [showConfirmModal, setShowConfirmModal] = useState<boolean>(false);

  const lookupUserIdRef = useRef<HTMLInputElement>(null);

  useEffect(() => {
    if (preselectedUserId) {
      handleLookupUser();
    } else if (lookupUserIdRef.current) {
      lookupUserIdRef.current.focus();
    }
  }, [preselectedUserId]);

  const clearMessages = () => {
    setSuccessMessage('');
    setGeneralError('');
  };

  const handleLookupUser = async () => {
    const userIdToLookup = preselectedUserId || lookupUserId.trim();
    
    if (!userIdToLookup) {
      setGeneralError('User ID can NOT be empty...');
      return;
    }

    setIsLookingUp(true);
    clearMessages();
    setCurrentUser(null);

    try {
      const response = await userSecurityService.getUserById(userIdToLookup);

      if (response.success && response.data) {
        const user = response.data;
        setCurrentUser(user);
        setSuccessMessage('Press PF5 key to delete this user ...');
      } else {
        setGeneralError('User ID NOT found...');
        setCurrentUser(null);
      }
    } catch (error) {
      console.error('Error looking up user:', error);
      setGeneralError('Unable to lookup User...');
    } finally {
      setIsLookingUp(false);
    }
  };

  const handleDeleteUser = async () => {
    if (!currentUser) {
      setGeneralError('User ID can NOT be empty...');
      return;
    }

    setIsLoading(true);
    clearMessages();

    try {
      const result: UserSecurityActionResult = await userSecurityService.deleteUserWithResult(
        currentUser.userId
      );

      if (result.success) {
        // Success message with green color (COUSR03C business rule)
        setSuccessMessage(`User ${currentUser.userId} has been deleted ...`);
        
        // Clear form after successful deletion (COUSR03C business rule)
        const deletedUserId = currentUser.userId;
        setCurrentUser(null);
        setLookupUserId('');

        // Notify parent component
        if (onUserDeleted) {
          onUserDeleted(deletedUserId);
        }

        // Focus back to User ID field
        setTimeout(() => {
          if (lookupUserIdRef.current) {
            lookupUserIdRef.current.focus();
          }
        }, 100);
      } else {
        if (result.message.includes('NOT found')) {
          setGeneralError('User ID NOT found...');
        } else {
          setGeneralError('Unable to Update User...');
        }
      }
    } catch (error) {
      console.error('Error deleting user:', error);
      setGeneralError('Unable to Update User...');
    } finally {
      setIsLoading(false);
    }
  };

  const handleClear = () => {
    setLookupUserId('');
    setCurrentUser(null);
    clearMessages();

    if (lookupUserIdRef.current) {
      lookupUserIdRef.current.focus();
    }
  };

  const handleExit = () => {
    if (onExit) {
      onExit();
    } else {
      router.push('/admin/dashboard');
    }
  };

  const handleKeyDown = (event: React.KeyboardEvent) => {
    switch (event.key) {
      case 'Enter':
        event.preventDefault();
        if (!currentUser) {
          handleLookupUser();
        }
        break;

      case 'F3':
        event.preventDefault();
        if (!isLoading) {
          handleExit();
        }
        break;

      case 'F4':
        event.preventDefault();
        if (!isLoading) {
          handleClear();
        }
        break;

      case 'F5':
        event.preventDefault();
        if (!isLoading && currentUser) {
          handleDeleteUser();
        }
        break;

      case 'F12':
        event.preventDefault();
        if (!isLoading) {
          handleExit();
        }
        break;

      default:
        // Allow normal key processing
        break;
    }
  };

  return (
    <div 
      className={`min-h-screen bg-gray-50 p-6 ${className}`}
      onKeyDown={handleKeyDown}
      tabIndex={-1}
    >
      <div className="max-w-4xl mx-auto">
        {/* Header */}
        <div className="mb-6">
          <h1 className="text-2xl font-bold text-gray-900 mb-2">
            CardDemo
          </h1>
          <h2 className="text-xl text-gray-700 mb-2">
            Delete a User
          </h2>
          <div className="flex justify-between text-sm text-gray-600">
            <span>COUSR03C - CU03</span>
            <span>{new Date().toLocaleDateString()} {new Date().toLocaleTimeString()}</span>
          </div>
        </div>

        {/* Success Message */}
        {successMessage && (
          <div className="bg-green-50 border border-green-200 rounded-md p-4 mb-6">
            <p className="text-sm text-green-800 font-medium">{successMessage}</p>
          </div>
        )}

        {/* General Error */}
        {generalError && (
          <div className="bg-red-50 border border-red-200 rounded-md p-4 mb-6">
            <p className="text-sm text-red-800">{generalError}</p>
          </div>
        )}

        {/* User Lookup Section */}
        {!preselectedUserId && (
          <div className="bg-white rounded-lg shadow-sm border border-gray-200 p-6 mb-6">
            <div className="flex gap-4 items-end">
              <div className="flex-1">
                <label htmlFor="lookupUserId" className="block text-sm font-medium text-gray-700 mb-1">
                  User ID:
                </label>
                <input
                  ref={lookupUserIdRef}
                  id="lookupUserId"
                  type="text"
                  value={lookupUserId}
                  onChange={(e) => {
                    setLookupUserId(e.target.value.toUpperCase());
                    clearMessages();
                  }}
                  placeholder="Enter User ID to lookup"
                  className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-indigo-500 focus:border-indigo-500"
                  disabled={isLookingUp || isLoading}
                  maxLength={8}
                />
              </div>
            </div>
          </div>
        )}

        {/* User Details Display - Only show if user is loaded */}
        {currentUser && (
          <div className="bg-white rounded-lg shadow-sm border border-gray-200 p-6 mb-6">
            <h2 className="text-lg font-medium text-gray-900 mb-4">User Details</h2>

            <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
              {/* User ID */}
              <div>
                <label className="block text-sm font-medium text-gray-700 mb-1">
                  User ID:
                </label>
                <input
                  type="text"
                  value={currentUser.userId}
                  readOnly
                  className="w-full px-3 py-2 border border-gray-300 rounded-md bg-gray-50"
                />
              </div>

              {/* First Name */}
              <div>
                <label className="block text-sm font-medium text-gray-700 mb-1">
                  First Name:
                </label>
                <input
                  type="text"
                  value={currentUser.firstName}
                  readOnly
                  className="w-full px-3 py-2 border border-gray-300 rounded-md bg-gray-50"
                />
              </div>

              {/* Last Name */}
              <div>
                <label className="block text-sm font-medium text-gray-700 mb-1">
                  Last Name:
                </label>
                <input
                  type="text"
                  value={currentUser.lastName}
                  readOnly
                  className="w-full px-3 py-2 border border-gray-300 rounded-md bg-gray-50"
                />
              </div>

              {/* User Type */}
              <div>
                <label className="block text-sm font-medium text-gray-700 mb-1">
                  User Type:
                </label>
                <input
                  type="text"
                  value={currentUser.userType}
                  readOnly
                  className="w-full px-3 py-2 border border-gray-300 rounded-md bg-gray-50"
                />
              </div>
            </div>

            {/* Warning Message */}
            <div className="mt-6 p-4 bg-red-50 border border-red-200 rounded-md">
              <div className="flex items-start">
                <div className="flex-shrink-0">
                  <svg className="h-5 w-5 text-red-400" viewBox="0 0 20 20" fill="currentColor">
                    <path fillRule="evenodd" d="M8.257 3.099c.765-1.36 2.722-1.36 3.486 0l5.58 9.92c.75 1.334-.213 2.98-1.742 2.98H4.42c-1.53 0-2.493-1.646-1.743-2.98l5.58-9.92zM11 13a1 1 0 11-2 0 1 1 0 012 0zm-1-8a1 1 0 00-1 1v3a1 1 0 002 0V6a1 1 0 00-1-1z" clipRule="evenodd" />
                  </svg>
                </div>
                <div className="ml-3">
                  <h3 className="text-sm font-medium text-red-800">
                    Warning: User Deletion
                  </h3>
                  <p className="mt-1 text-sm text-red-700">
                    This action will permanently delete the user and cannot be undone.
                  </p>
                </div>
              </div>
            </div>
          </div>
        )}

        {/* Action Buttons */}
        <div className="flex flex-wrap gap-4 mb-6">
          {!currentUser ? (
            <>
              <button
                onClick={handleLookupUser}
                disabled={isLookingUp || !lookupUserId.trim()}
                className="flex-1 sm:flex-none min-w-[120px] bg-indigo-600 text-white px-4 py-2 rounded-md hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-indigo-500 disabled:opacity-50 disabled:cursor-not-allowed"
              >
                {isLookingUp ? 'Looking up...' : 'Lookup (Enter)'}
              </button>
              
              <button
                onClick={handleClear}
                disabled={isLookingUp}
                className="flex-1 sm:flex-none min-w-[120px] bg-gray-600 text-white px-4 py-2 rounded-md hover:bg-gray-700 focus:outline-none focus:ring-2 focus:ring-gray-500 disabled:opacity-50 disabled:cursor-not-allowed"
              >
                Clear (F4)
              </button>
              
              <button
                onClick={handleExit}
                disabled={isLookingUp}
                className="flex-1 sm:flex-none min-w-[120px] bg-gray-600 text-white px-4 py-2 rounded-md hover:bg-gray-700 focus:outline-none focus:ring-2 focus:ring-gray-500 disabled:opacity-50 disabled:cursor-not-allowed"
              >
                Exit (F3)
              </button>
            </>
          ) : (
            <>
              <button
                onClick={handleDeleteUser}
                disabled={isLoading}
                className="flex-1 sm:flex-none min-w-[140px] bg-red-600 text-white px-4 py-2 rounded-md hover:bg-red-700 focus:outline-none focus:ring-2 focus:ring-red-500 disabled:opacity-50 disabled:cursor-not-allowed"
              >
                {isLoading ? 'Deleting...' : 'Delete User (F5)'}
              </button>
              
              <button
                onClick={handleClear}
                disabled={isLoading}
                className="flex-1 sm:flex-none min-w-[120px] bg-gray-600 text-white px-4 py-2 rounded-md hover:bg-gray-700 focus:outline-none focus:ring-2 focus:ring-gray-500 disabled:opacity-50 disabled:cursor-not-allowed"
              >
                Clear (F4)
              </button>
              
              <button
                onClick={handleExit}
                disabled={isLoading}
                className="flex-1 sm:flex-none min-w-[120px] bg-gray-600 text-white px-4 py-2 rounded-md hover:bg-gray-700 focus:outline-none focus:ring-2 focus:ring-gray-500 disabled:opacity-50 disabled:cursor-not-allowed"
              >
                Exit (F3)
              </button>
            </>
          )}
        </div>

        {/* Function Key Instructions */}
        <div className="bg-gray-50 border border-gray-200 rounded-md p-4">
          <h3 className="text-sm font-medium text-gray-900 mb-2">Instructions</h3>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-2 text-xs text-gray-600">
            <div><span className="font-medium">Enter:</span> Lookup user</div>
            <div><span className="font-medium">F3:</span> Exit to Admin Menu</div>
            <div><span className="font-medium">F4:</span> Clear all fields</div>
            <div><span className="font-medium">F5:</span> Delete user</div>
            <div><span className="font-medium">F12:</span> Cancel/Exit</div>
          </div>
        </div>
      </div>
    </div>
  );
};

export default UserDeleteComponent;