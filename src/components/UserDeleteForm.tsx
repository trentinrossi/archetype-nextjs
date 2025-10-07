'use client';

import React, { useState, useEffect } from 'react';
import { Button } from '@/components/ui/Button';
import { Input } from '@/components/ui/Input';
import { UserSecurityDTO } from '@/types/user';
import { userService } from '@/services/userService';

interface UserDeleteFormProps {
  preSelectedUserId?: string;
  onSuccess: (message: string) => void;
  onExit: () => void;
}

export function UserDeleteForm({ preSelectedUserId, onSuccess, onExit }: UserDeleteFormProps) {
  const [searchUserId, setSearchUserId] = useState(preSelectedUserId || '');
  const [currentUser, setCurrentUser] = useState<UserSecurityDTO | null>(null);
  const [message, setMessage] = useState('');
  const [loading, setLoading] = useState(false);

  // COUSR03C business rule: Auto-load user if pre-selected
  useEffect(() => {
    if (preSelectedUserId) {
      handleLookupUser();
    }
  }, [preSelectedUserId]);

  // COUSR03C business rule: Handle ENTER key (lookup user)
  const handleLookupUser = async () => {
    if (!searchUserId.trim()) {
      setMessage('User ID can NOT be empty...');
      return;
    }

    setLoading(true);
    setMessage('');

    try {
      const user = await userService.getUserById(searchUserId);
      setCurrentUser(user);
      setMessage('Press PF5 key to delete this user ...');
    } catch (error) {
      const errorMessage = error instanceof Error ? error.message : 'Unable to lookup User...';
      setMessage(errorMessage);
      setCurrentUser(null);
    } finally {
      setLoading(false);
    }
  };

  // COUSR03C business rule: Handle PF5 key (delete user)
  const handleDelete = async () => {
    if (!currentUser) {
      setMessage('Please lookup a user first');
      return;
    }

    if (!searchUserId.trim()) {
      setMessage('User ID can NOT be empty...');
      return;
    }

    setLoading(true);
    setMessage('');

    try {
      const response = await userService.deleteUser(currentUser.userId);
      
      // Clear form after successful deletion (COUSR03C business rule)
      setSearchUserId('');
      setCurrentUser(null);
      setMessage(response.message);
      onSuccess(response.message);
    } catch (error) {
      const errorMessage = error instanceof Error ? error.message : 'Unable to Update User...';
      setMessage(errorMessage);
    } finally {
      setLoading(false);
    }
  };

  // COUSR03C business rule: Clear form (PF4 key)
  const handleClear = () => {
    setSearchUserId('');
    setCurrentUser(null);
    setMessage('');
  };

  // Handle invalid key press
  const handleInvalidKey = () => {
    setMessage('Invalid key pressed');
  };

  return (
    <div className="min-h-screen bg-gray-100 p-4">
      <div className="bg-white rounded-lg shadow-md p-6 max-w-2xl mx-auto">
        {/* Screen Header - COUSR03C layout */}
        <div className="mb-6 text-center border-b pb-4">
          <h1 className="text-2xl font-bold text-gray-800 mb-2">CARDDEMO</h1>
          <h2 className="text-lg text-gray-600 mb-4">Delete a User</h2>
          <div className="text-sm text-gray-500 space-y-1">
            <div>Program: COUSR03C | Transaction: CU03</div>
            <div>{new Date().toLocaleDateString('en-US', { 
              month: '2-digit', 
              day: '2-digit', 
              year: '2-digit' 
            })} {new Date().toLocaleTimeString('en-US', { 
              hour12: false 
            })}</div>
          </div>
        </div>

        {/* User Lookup Section */}
        <div className="mb-6 p-4 bg-gray-50 rounded">
          <div className="flex gap-2 items-end">
            <div className="flex-1">
              <Input
                label="User ID"
                type="text"
                value={searchUserId}
                onChange={(e) => setSearchUserId(e.target.value.toUpperCase().substring(0, 8))}
                maxLength={8}
                placeholder="Enter User ID to delete"
                autoFocus={!preSelectedUserId}
              />
            </div>
            <Button
              onClick={handleLookupUser}
              disabled={loading}
              className="mb-0"
            >
              Lookup (ENTER)
            </Button>
          </div>
        </div>

        {/* User Details Display */}
        {currentUser && (
          <div className="mb-6 p-4 bg-red-50 border border-red-200 rounded">
            <h3 className="font-medium text-red-800 mb-4">User to be deleted:</h3>
            <div className="grid grid-cols-2 gap-4 text-sm">
              <div>
                <label className="font-medium text-gray-700">User ID:</label>
                <div className="font-mono bg-white p-2 rounded border">
                  {currentUser.userId}
                </div>
              </div>
              <div>
                <label className="font-medium text-gray-700">User Type:</label>
                <div className="bg-white p-2 rounded border">
                  {currentUser.userTypeDisplayName}
                </div>
              </div>
              <div>
                <label className="font-medium text-gray-700">Status:</label>
                <div className="bg-white p-2 rounded border">
                  <span className={`px-2 py-1 rounded text-xs ${
                    currentUser.active ? 'bg-green-100 text-green-800' : 'bg-red-100 text-red-800'
                  }`}>
                    {currentUser.active ? 'Active' : 'Inactive'}
                  </span>
                </div>
              </div>
              <div>
                <label className="font-medium text-gray-700">Program:</label>
                <div className="bg-white p-2 rounded border">
                  {currentUser.programName}
                </div>
              </div>
            </div>
            
            <div className="mt-4 p-3 bg-red-100 border border-red-300 rounded">
              <p className="text-red-800 text-sm font-medium">
                ⚠️ Warning: This action cannot be undone. The user will be permanently deleted.
              </p>
            </div>
          </div>
        )}

        {/* Message area */}
        {message && (
          <div className={`mb-4 p-3 rounded text-sm ${
            message.includes('deleted') || message.includes('successful')
              ? 'bg-green-100 text-green-700' 
              : message.includes('Press PF5')
              ? 'bg-blue-100 text-blue-700'
              : 'bg-red-100 text-red-700'
          }`}>
            {message}
          </div>
        )}

        {/* Function Keys - COUSR03C business rules */}
        <div className="space-y-2">
          {currentUser && (
            <Button
              onClick={handleDelete}
              disabled={loading}
              variant="destructive"
              className="w-full mb-2"
            >
              {loading ? 'Deleting User...' : 'Delete User (PF5)'}
            </Button>
          )}

          <div className="grid grid-cols-3 gap-2">
            <Button
              variant="secondary"
              onClick={onExit}
              disabled={loading}
            >
              Exit (PF3)
            </Button>

            <Button
              variant="outline"
              onClick={handleClear}
              disabled={loading}
            >
              Clear (PF4)
            </Button>

            <Button
              variant="outline"
              onClick={onExit}
              disabled={loading}
            >
              Cancel (PF12)
            </Button>
          </div>

          <Button
            variant="destructive"
            onClick={handleInvalidKey}
            disabled={loading}
            className="w-full"
          >
            Invalid Key
          </Button>
        </div>

        {/* Instructions */}
        <div className="mt-6 text-xs text-gray-500 text-center space-y-1">
          <p>Enter User ID and press Lookup to load user details</p>
          <p>Review the user information carefully, then press Delete User (PF5)</p>
          <p>Use Exit (PF3) or Cancel (PF12) to return without deleting</p>
          <p className="text-red-600 font-medium">⚠️ Deletion is permanent and cannot be undone</p>
        </div>
      </div>
    </div>
  );
}