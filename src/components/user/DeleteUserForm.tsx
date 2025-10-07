'use client';

import React, { useState, useEffect } from 'react';
import { Button } from '@/components/ui/Button';
import { UserSecurityDTO } from '@/types/user';
import { userService } from '@/services/userService';

interface DeleteUserFormProps {
  selectedUserId?: string;
  onUserDeleted: (userId: string) => void;
  onExit: () => void;
}

export function DeleteUserForm({ selectedUserId, onUserDeleted, onExit }: DeleteUserFormProps) {
  const [formData, setFormData] = useState({
    userId: selectedUserId || '',
    firstName: '',
    lastName: '',
    userType: ''
  });
  const [message, setMessage] = useState('');
  const [messageType, setMessageType] = useState<'success' | 'error' | 'info'>('info');
  const [loading, setLoading] = useState(false);
  const [userLoaded, setUserLoaded] = useState(false);

  // Load user data if selectedUserId is provided
  useEffect(() => {
    if (selectedUserId && !userLoaded) {
      handleEnterKey();
    }
  }, [selectedUserId, userLoaded]);

  // Handle User ID input change
  const handleUserIdChange = (value: string) => {
    // Convert to uppercase and limit to 8 characters (COBOL business logic)
    const processedValue = value.toUpperCase().slice(0, 8);
    setFormData(prev => ({
      ...prev,
      userId: processedValue
    }));

    // Clear message when user starts typing
    if (message) {
      setMessage('');
    }
  };

  // Handle ENTER key press - lookup user (COUSR03C business logic)
  const handleEnterKey = async () => {
    if (!formData.userId.trim()) {
      setMessage('User ID can NOT be empty...');
      setMessageType('error');
      return;
    }

    setLoading(true);
    setMessage('');

    try {
      const user = await userService.getUserById(formData.userId);
      
      // Populate form with user data (read-only display)
      setFormData(prev => ({
        ...prev,
        firstName: user.userType === 'ADMIN' ? 'System' : 'General',
        lastName: user.userType === 'ADMIN' ? 'Administrator' : 'User',
        userType: user.userType === 'ADMIN' ? 'A' : 'U'
      }));
      
      setUserLoaded(true);
      setMessage('Press PF5 key to delete this user ...');
      setMessageType('info');

    } catch (error) {
      const errorMessage = error instanceof Error ? error.message : 'Unable to lookup User...';
      
      if (errorMessage.includes('not found')) {
        setMessage('User ID NOT found...');
      } else {
        setMessage(errorMessage);
      }
      setMessageType('error');
      
      // Clear form data except userId
      setFormData(prev => ({
        ...prev,
        firstName: '',
        lastName: '',
        userType: ''
      }));
      setUserLoaded(false);
    } finally {
      setLoading(false);
    }
  };

  // Handle PF5 key press - delete user (COUSR03C business logic)
  const handlePF5Key = async () => {
    if (!formData.userId.trim()) {
      setMessage('User ID can NOT be empty...');
      setMessageType('error');
      return;
    }

    if (!userLoaded) {
      setMessage('Please lookup a user first');
      setMessageType('error');
      return;
    }

    setLoading(true);
    setMessage('');

    try {
      await userService.deleteUser(formData.userId);
      
      // Success - clear form and show success message (COBOL business logic)
      const deletedUserId = formData.userId;
      setFormData({
        userId: '',
        firstName: '',
        lastName: '',
        userType: ''
      });
      setUserLoaded(false);
      setMessage(`User ${deletedUserId} has been deleted ...`);
      setMessageType('success');
      
      onUserDeleted(deletedUserId);

    } catch (error) {
      const errorMessage = error instanceof Error ? error.message : 'Unable to Update User...';
      
      if (errorMessage.includes('not found')) {
        setMessage('User ID NOT found...');
      } else {
        setMessage(errorMessage);
      }
      setMessageType('error');
    } finally {
      setLoading(false);
    }
  };

  // Handle PF3 key press - exit
  const handlePF3Key = () => {
    onExit();
  };

  // Handle PF4 key press - clear screen
  const handlePF4Key = () => {
    setFormData({
      userId: '',
      firstName: '',
      lastName: '',
      userType: ''
    });
    setUserLoaded(false);
    setMessage('');
  };

  // Handle PF12 key press - cancel
  const handlePF12Key = () => {
    onExit();
  };

  // Handle form submission
  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    if (userLoaded) {
      // If user is loaded, PF5 action (delete)
      handlePF5Key();
    } else {
      // If user not loaded, ENTER action (lookup)
      handleEnterKey();
    }
  };

  // Handle key press events (simulating COBOL function keys)
  const handleKeyPress = (e: React.KeyboardEvent) => {
    if (e.key === 'F3' || (e.altKey && e.key === '3')) {
      e.preventDefault();
      handlePF3Key();
    } else if (e.key === 'F4' || (e.altKey && e.key === '4')) {
      e.preventDefault();
      handlePF4Key();
    } else if (e.key === 'F5' || (e.altKey && e.key === '5')) {
      e.preventDefault();
      handlePF5Key();
    } else if (e.key === 'F12' || (e.altKey && e.key === '12')) {
      e.preventDefault();
      handlePF12Key();
    }
  };

  return (
    <div className="min-h-screen bg-gray-900 text-green-400 font-mono">
      {/* Header */}
      <div className="bg-gray-800 p-4 border-b border-green-400">
        <div className="flex justify-between items-center">
          <div>
            <h1 className="text-xl font-bold">CARDDEMO</h1>
            <h2 className="text-lg">Delete a User</h2>
          </div>
          <div className="text-right">
            <div>TRAN: CU03</div>
            <div>PGM: COUSR03C</div>
            <div>{new Date().toLocaleDateString('en-US', { 
              month: '2-digit', 
              day: '2-digit', 
              year: '2-digit' 
            })}</div>
            <div>{new Date().toLocaleTimeString('en-US', { 
              hour12: false,
              hour: '2-digit',
              minute: '2-digit',
              second: '2-digit'
            })}</div>
          </div>
        </div>
      </div>

      {/* Main form area */}
      <div className="p-8">
        <form onSubmit={handleSubmit} onKeyDown={handleKeyPress} className="space-y-6">
          {/* User ID field */}
          <div className="flex items-center space-x-4">
            <label htmlFor="userId" className="w-32 text-right">
              User ID:
            </label>
            <input
              id="userId"
              type="text"
              value={formData.userId}
              onChange={(e) => handleUserIdChange(e.target.value)}
              className="bg-gray-800 border border-green-400 text-green-400 px-2 py-1 w-32 focus:outline-none focus:border-yellow-400"
              maxLength={8}
              autoFocus={!userLoaded}
              disabled={loading}
            />
            <span className="text-gray-500">(Max 8 characters)</span>
          </div>

          {/* First Name field (read-only) */}
          <div className="flex items-center space-x-4">
            <label htmlFor="firstName" className="w-32 text-right">
              First Name:
            </label>
            <input
              id="firstName"
              type="text"
              value={formData.firstName}
              className="bg-gray-700 border border-gray-600 text-gray-400 px-2 py-1 w-64 cursor-not-allowed"
              readOnly
            />
            <span className="text-gray-500">(Display only)</span>
          </div>

          {/* Last Name field (read-only) */}
          <div className="flex items-center space-x-4">
            <label htmlFor="lastName" className="w-32 text-right">
              Last Name:
            </label>
            <input
              id="lastName"
              type="text"
              value={formData.lastName}
              className="bg-gray-700 border border-gray-600 text-gray-400 px-2 py-1 w-64 cursor-not-allowed"
              readOnly
            />
            <span className="text-gray-500">(Display only)</span>
          </div>

          {/* User Type field (read-only) */}
          <div className="flex items-center space-x-4">
            <label htmlFor="userType" className="w-32 text-right">
              User Type:
            </label>
            <input
              id="userType"
              type="text"
              value={formData.userType}
              className="bg-gray-700 border border-gray-600 text-gray-400 px-2 py-1 w-12 cursor-not-allowed"
              readOnly
            />
            <span className="text-gray-500">(A=Admin, U=User)</span>
          </div>

          {/* Action buttons */}
          <div className="flex space-x-4 mt-8">
            <Button
              type="submit"
              disabled={loading}
              className="bg-green-700 hover:bg-green-600 text-white"
            >
              {loading ? 'Processing...' : userLoaded ? 'ENTER - Confirm Delete' : 'ENTER - Lookup User'}
            </Button>
            
            <Button
              type="button"
              onClick={handlePF3Key}
              variant="secondary"
              disabled={loading}
              className="bg-gray-700 hover:bg-gray-600 text-white"
            >
              PF3 - Exit
            </Button>
            
            <Button
              type="button"
              onClick={handlePF4Key}
              variant="outline"
              disabled={loading}
              className="bg-gray-700 hover:bg-gray-600 text-white border-gray-500"
            >
              PF4 - Clear
            </Button>
            
            <Button
              type="button"
              onClick={handlePF5Key}
              disabled={loading || !userLoaded}
              variant="destructive"
              className="bg-red-700 hover:bg-red-600 text-white disabled:opacity-50"
            >
              PF5 - Delete User
            </Button>
            
            <Button
              type="button"
              onClick={handlePF12Key}
              variant="secondary"
              disabled={loading}
              className="bg-gray-700 hover:bg-gray-600 text-white"
            >
              PF12 - Cancel
            </Button>
          </div>

          {/* Message area */}
          {message && (
            <div className={`mt-6 p-3 border ${
              messageType === 'success' ? 'border-green-400 text-green-400' :
              messageType === 'error' ? 'border-red-400 text-red-400' :
              'border-yellow-400 text-yellow-400'
            } bg-gray-800`}>
              {message}
            </div>
          )}

          {/* Warning message for delete operation */}
          {userLoaded && (
            <div className="mt-6 p-3 border border-red-400 text-red-400 bg-gray-800">
              <strong>WARNING:</strong> This action will permanently delete the user. This cannot be undone.
            </div>
          )}

          {/* Instructions */}
          <div className="mt-8 text-gray-500 text-sm">
            <p>Instructions:</p>
            <ul className="list-disc list-inside mt-2 space-y-1">
              <li>Enter User ID and press ENTER to lookup user</li>
              <li>Review user details carefully</li>
              <li>Press PF5 to permanently delete the user</li>
              <li>Press PF3 to exit to Admin Menu</li>
              <li>Press PF4 to clear all fields</li>
              <li>Press PF12 to cancel without deleting</li>
            </ul>
          </div>
        </form>
      </div>
    </div>
  );
}